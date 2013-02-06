-ifndef(__ERLSON_HRL__).
-define(__ERLSON_HRL__,1).

-compile({parse_transform, erlson_pt}).


-define(erlson_default(X, Default),
    try
        X
    catch error:'erlson_not_found' ->
        Default
    end
).

-define(erlson_default(X), ?erlson_default(X, 'undefined')).


-endif.
