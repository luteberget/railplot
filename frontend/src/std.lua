function check(x, msg)
    if x then return {} else error(msg) end
end

function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

function concat(a,b)
    z = {}
    n = 0
    for _,v in ipairs(a) do n=n+1; z[n]=v end
    for _,v in ipairs(b) do n=n+1; z[n]=v end
    return z
end

function extend(a,b)
    n = #a
    for _,v in ipairs(b) do n=n+1; a[n]=v end
    return a
end
