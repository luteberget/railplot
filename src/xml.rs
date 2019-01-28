use failure::ResultExt;

pub fn lua_to_json<'l>(ctx: rlua::Context<'l>, x :rlua::Value) -> Result<serde_json::Value, failure::Error> {
    match x {
        rlua::Value::Nil => Ok(serde_json::Value::Null),
        rlua::Value::Boolean(b) => Ok(serde_json::Value::Bool(b)),
        rlua::Value::LightUserData(_) => Ok(serde_json::Value::String(format!("<lightuserdata>"))),
        rlua::Value::Function(_) => Ok(serde_json::Value::String(format!("<function>"))),
        rlua::Value::Thread(_) => Ok(serde_json::Value::String(format!("<thread>"))),
        rlua::Value::UserData(_) => Ok(serde_json::Value::String(format!("<userdata>"))),
        rlua::Value::Error(_) => Ok(serde_json::Value::String(format!("<error>"))),
        rlua::Value::String(s) => Ok(serde_json::Value::String(s.to_str()?.into())),
        rlua::Value::Number(f) => Ok(serde_json::Number::from_f64(f)
               .map(serde_json::Value::Number)
               .unwrap_or_else(|| serde_json::Value::String(format!("<NaN>")))),
        rlua::Value::Integer(i) => Ok(serde_json::Value::Number(i.into())),
        rlua::Value::Table(t) => {
            if let rlua::Value::Nil = t.get::<i64,rlua::Value>(1)? {
                let mut map = serde_json::Map::new();
                for r in t.pairs() {
                    let (k,v) = r?;
                    let json_val :serde_json::Value = lua_to_json(ctx, v)?;
                    map.insert(k,json_val);
                }
                Ok(serde_json::Value::Object(map))
            } else {
                Ok(serde_json::Value::Array(t.sequence_values()
                                            .map(|v| lua_to_json(ctx, v?))
                                            .collect::<Result<Vec<_>,_>>()?))
            }
        },
    }
}

pub fn json_to_lua<'lua>(ctx: rlua::Context<'lua>, x :serde_json::Value) -> Result<rlua::Value<'lua>,failure::Error> {
    match x {
        serde_json::Value::Null => Ok(rlua::Value::Nil),
        serde_json::Value::Bool(b) => Ok(rlua::Value::Boolean(b)),
        serde_json::Value::Number(n) =>  Ok(rlua::Value::Number(n.as_f64().unwrap())),
        serde_json::Value::String(s) =>  Ok(rlua::Value::String(ctx.create_string(&s)?)),
        serde_json::Value::Array(b) => {
            //let t = ctx.create_sequence_from(b.into_iter().map(|v| json_to_lua(ctx, v).unwrap()))?;
            let t = ctx.create_table()?;
            for (i,x) in b.into_iter().enumerate() {
                t.set(i+1,json_to_lua(ctx,x)?)?;
            }
            Ok(rlua::Value::Table(t))
        },
        serde_json::Value::Object(b) =>  {
            let t = ctx.create_table()?;
            for (k,v) in b {
                t.set(k,json_to_lua(ctx,v)?)?;
            }
            Ok(rlua::Value::Table(t))
        },
    }
}

pub fn open_xml(filename :&str) -> Result<minidom::Element, failure::Error> {
    use std::fs::File;
    //use std::io::Read;
    use std::io::BufReader;

    let f = File::open(filename)
        .with_context(|e| format!("Could not open file {}: {}", filename,e)) ?;
    let f = BufReader::new(f);
    let e = minidom::Element::from_reader(&mut quick_xml::Reader::from_reader(f));

    Ok(e?)
}

pub fn load_xml_to_json(filename :&str, arrays :&[String] ) -> Result<serde_json::Value, failure::Error> {

    let el = open_xml(filename).with_context(|e| format!("XML parse error: {}", e))?;
    xml_to_json(&el,arrays)
}

pub fn xml_to_json(el :&minidom::Element, arrays :&[String] ) -> Result<serde_json::Value, failure::Error> {

    fn simple_value(v :&str) -> serde_json::Value {
            if let Ok(i) = v.parse::<i64>() {
                i.into()
            } else if let Ok(i) = v.parse::<f64>() {
                i.into()
            } else {
                v.into()
            }
    }

    //fn convert_alt(e :&minidom::Element) -> Result<serde_json::Value, failure::Error> {
    //    // let name = e.name();  ignore the name here, use children's names.

    //    // Spark convention

    //    let mut obj = serde_json::json!({});

    //    // first, do we have no attrs and one child element type?
    //    let one_elem_type = if let Some(c) = e.children().nth(0) { 
    //        if e.attrs().count() == 0 && e.children().all(|x| x.name() == c.name()) {
    //            true 
    //        } else { false } } else { false };

    //    if one_elem_type {

    //    }

    //    for (k,v) in e.attrs() {
    //        obj.as_object_mut().unwrap().insert(k.into(), simple_value(v));
    //    }

    //    Ok(obj)
    //}


    fn convert_alt(e :&minidom::Element, arrays :&[String]) -> Result<serde_json::Value, failure::Error> {
        // loop over children and attributes
        let mut json = serde_json::json!({"_elem": e.name()});
        for (k,v) in e.attrs() { 
            json.as_object_mut().unwrap().insert(k.into(), simple_value(v));
        }
        for c in e.children() {
            if arrays.iter().any(|x| x == c.name()) {
                // we found "tracks"; make that into an array
                for x in c.children() {
                    json.as_object_mut().unwrap().entry(c.name())
                        .or_insert_with(|| serde_json::json!([]))
                        .as_array_mut().unwrap().push( convert_alt(x, arrays)? );
                }
            } else {
                // just add the value as a regular property
                match json.as_object_mut().unwrap().entry(c.name()) {
                    serde_json::map::Entry::Occupied(mut occupied) => {
                        let old = occupied.insert(serde_json::json!([]));
                        let a = occupied.get_mut().as_array_mut().unwrap();
                        a.push(old);
                        a.push(convert_alt(c,arrays)?);
                    }, 
                    serde_json::map::Entry::Vacant(v) => {
                            v.insert(convert_alt(c, arrays)?);
                    },
                }
            }
        }
        Ok(json)
    }

    //fn convert(e :&minidom::Element, arrays :&[String]) -> Result<serde_json::Value, failure::Error> {
    //    // { name: "railml",
    //    //   attrs: { x: y },
    //    //   children: {} }

    //    let name = e.name();
    //    let mut attrs = serde_json::json!({});
    //    for (k,v) in e.attrs() { 
    //        attrs.as_object_mut().unwrap().insert(k.into(), simple_value(v));
    //    }
    //    let mut children = serde_json::json!([]);
    //    for c in e.children() {
    //        children.as_array_mut().unwrap().push(convert(c, arrays)?);
    //    }
    //    Ok(serde_json::json!({"name":name, "attrs": attrs, "children": children}))
    //}

    convert_alt(&el, arrays)
}
