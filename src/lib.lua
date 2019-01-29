function check_warn(x,msg) if x then return {} else log_warn(msg) end end
function check_info(x,msg) if x then return {} else log_info(msg) end end
function check_debug(x,msg) if x then return {} else log_debug(msg) end end
function check_trace(x,msg) if x then return {} else log_trace(msg) end end

args_ok = true

function check_args(x, msg)
    if x then return true 
    else print_help(msg) args_ok = false return false end
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
    if b ~= nil then
        n = #a
        for _,v in ipairs(b) do n=n+1; a[n]=v end
    end
    return a
end


