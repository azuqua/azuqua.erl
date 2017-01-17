-record(api_state, {key = <<>>, secret = <<>>, opts = #{}, promises=#{},
                    flos = #{}}).
-record(promise, {return, data, cref, state}).
-record(request, {path = <<>>, qs = <<>>, body = <<>>, headers = [],
                  method = get}).
