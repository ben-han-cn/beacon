-define(REMOTE_SLAVE_CREATE_TIMEOUT, 3000).
-record(beacon_cmd, {id,
                     name, % atom
                     args}).   % tuples {[k, v], [k, v]....}


