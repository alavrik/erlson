[![Build Status](https://travis-ci.org/alavrik/erlson.png)](https://travis-ci.org/alavrik/erlson)


**Update (2014-11-16): Erlson syntax now uses brackets instead of curly braces.

Erlang 17 introduced a new built-in `maps` type. Maps syntax conflicts with the
original Erlson syntax that relied on curly braces.

For this reason, we changed Erlson syntax to use brackets instead. Such change
eliminates grammar conflicts and makes Erlson syntax look sufficiently different
from maps.

This syntax change is backward compatible. The old syntax is still supported
with Erlang R16 and earlier releases.


Erlson - Erlang Simple Object Notation
======================================

Erlson is a dynamic name-value dictionary data type for Erlang.

Erlson dictionaries come with a convenient syntax and can be directly converted
to and from JSON.

Examples:

```erlang
    % create an empty dictionary
    X = #[],

    % associate fields 'foo' with 1, 'bar' with "abc" and 'fum' with 'true'
    D = #[foo = 1, bar = "abc", fum],

    % access dictionary element
    1 = D.foo,

    % add nested dictionaries to dictionary D
    D1 = D#[baz = #[fum = #[i = 0]]],

    % access elements of the nested dictionary
    0 = D1.baz.fum.i,

    % modify elements of the nested dictionary
    D2 = D1#[baz.fum.i = 100, baz.fum.j = "new nested value"].

    ...

    % convert Erlson dictionary to JSON iolist()
    erlson:to_json(D2).

    % create Erlson dictionary from JSON iolist()
    D = erlson:from_json(Json).

    ...

    % create Erlson dictionary from a proplist
    D = erlson:from_proplist(L).

    % create nested Erlson dictionary from a nested proplist
    D = erlson:from_nested_proplist(L).

    % create nested Erlson dictionary from a nested proplist up to the maximum
    % depth of 2
    D = erlson:from_nested_proplist(L, 2).
```


Other API functions
-------------------

Parsing and generating top-level JSON arrays.

    ErlsonList = erlson:list_from_json_array(<JSON array represented as iolist()>)

    JsonArray = erlson:list_to_json_array(ErlsonList)


Accessing fields dynamically (i.e. without using Erlson syntax). The main
difference between Erlson field access syntax and `get_value/1` is that
`get_value/1` returns `undefined` if the field is missing whereas Erlson will
crash.

    erlson:get_value(Path, Dict)  % alias for get_value(Path, 'undefined')

    erlson:get_value(Path, Dict, Default)

    % Path :: atom() | [atom()]


Macros similar to `get_value/2,3` functions for gracefully handling missing
fields but still using Erlson syntax for accessing them. Example:

    -include_lib("erlson/include/erlson.hrl").

    ...

    ?erlson_default(X.foo.bar)

    ?erlson_default(X.foo.bar, Default)


General properties
------------------

- Erlson dictionaries contain zero or more Name->Value associations (fields),
  where each Name is `atom()` or `binary()` and Value can be of `any()` type.

- Name->Value associations are unique. If a new association is created for the
  existing Name, the old value will be replaced by the new value.

- Erlson dictionaries can be nested.



Runtime properties
------------------

- Only fields with `atom()` names can be accessed using the Erlson dictionary
  syntax.

- Unlike Erlang records, Erlson dictionaries can be dynamically constructed
  without any static type declarations.

- At runtime, Erlson dictionaries are represented as a list of {Name, Value}
  tuples ordered by Name. This way, each Erlson dictionary is a valid `proplist`
  and `orddict` in terms of the correspondent stdlib modules.

- Erlson dictionaries (dictionary syntax) can't be used as patterns and in guard
  expressions. An error message will be returned by the Erlang compiler Erlson
  syntax is used in pattern-matching or guard contexts.

- Erlson dictionaries can be used in both compiled modules and Erlang
  interactive shell.


Erlson and JSON
---------------

- Each valid JSON object can be converted to correspondent Erlson dictionary.

- An Erlson dictionary can be converted to JSON if it follows JSON data model.

- JSON->Erlson->JSON conversion produces an equivalent JSON object (fields may
  be reordered). The only exception is `{}` (empty JSON object), because it can
  not be directly represented in Erlson when loaded from JSON.

- There is one-to-one mapping between JSON and Erlang/Erlson values:

   - JSON object <-> Erlson dictionary
   - JSON array  <-> Erlang list
   - JSON number <-> Erlang `number()` (i.e. floats and integers)
   - JSON true | false <-> Erlang `boolean()`
   - JSON string value <-> Erlang `binary()`
   - JSON null <-> Erlang atom `undefined`

- JSON field names are decoded using the following function:

    ```erlang
    decode_json_field_name(N) ->
        try binary_to_existing_atom(N, utf8)
        catch
            error:badarg -> N
        end.
    ```

- The `erlson:to_json` function supports quoted JSON values represented as
  `{json, iodata()}` Erlang terms. The function will write the `iodata()` part
  of this term directly into JSON output. For example, `{json, "{}"}` Erlson
  value will turn into empty JSON object.

- Erlson uses Erlang built-in `binary_to_existing_atom/1` function for
  converting JSON field names into Erlang atoms. This is to prevent the VM from
  running out of atom space on malicious JSON input.

  Generally, everything should just work out of the box in compiled projects.
  However, if you are using Erlson from Erlang shell, not all of your modules
  and correspondent atoms may be loaded. In this case, JSON field names for
  unknown atoms will be represented as binaries after parsing. To fix that, you
  can load all the necessary modules. This can be done, for example, by running
  this command:

      Modules = [application:get_key(A, modules) || A <- application:loaded_applications()],
      [ code:load(M) || lists:append(Modules) ].


Erlson and property lists
-------------------------

Property list can be converted to Erlson dictionaries using the
`erlson:from_proplist` function and its variations.

Erlson dictionaries can also be used for property lists construction. Using
Erlson, proplists look much cleaner. For example, compare

```erlang
{application, erlson,
 [{description, "Erlang Simple Object Notation"},
  {vsn, git},
  {modules, []},
  {applications, [kernel, stdlib]},
  {env, []}]}.
```

and

```erlang
{application, erlson,
 #[description = "Erlang Simple Object Notation",
   vsn = git,
   modules = [],
   applications = [kernel, stdlib],
   env = []]].
```


Usage instructions
------------------

For compiled modules that use Erlson syntax, the Erlson library header must be
included:

```erlang
    -include_lib("erlson/include/erlson.hrl").
```

When rebar is used as a build tool, it should be configured to use
"erlson_rebar_plugin". In order to do that, add the following lines to the
project's "rebar.config" file:

```erlang
    {plugins, [erlson_rebar_plugin]}. % for newer rebar
    {rebar_plugins, [erlson_rebar_plugin]}. % for older rebar

    {deps,
        [
            {erlson, "", {git, "https://github.com/alavrik/erlson.git", {branch, "master"}}}
        ]}.
```

In order to use Erlson syntax from Erlang shell, run the following command (e.g.
include it in `.erlang` file):

```erlang
    erlson:init().
```


Erlang compatibility
--------------------

Erlson relies on modified versions of Erlang parsers taken from correspondent
Erlang/OTP releases. While Erlson is fully compatible with R13, R14, R15, R16,
R17 Erlang releases, compatibility between Erlson and future Erlang versions can
not be guaranteed.

Compatibility *can* break if future Erlang versions introduces new unrelated
syntax elements conflicting with Erlson grammar which will make Erlson
completely unusable in its current form. In response to that, Erlson may adjust
its grammar to remain compatible.


Dependencies
------------

Erlson relies on `mochijson2` library for JSON encoding and decoding. It comes
as a part of [Mochiweb](https://github.com/mochi/mochiweb). Erlson doesn't
automatically include it, but if you wish to do it with a rebar-enabled project,
add it as dependency in your `rebar.config`. For example:

```erlang
    {deps,
        [
            % we need Mochiweb for mochijson2
            {mochiweb, "", {git, "https://github.com/mochi/mochiweb.git", {branch, "master"}}}
        ]}.
```


Authors
-------

Erlson is created by Anton Lavrik <alavrik@piqi.org>.


License
-------

Erlson is distributed under the terms of a MIT license. See the LICENSE file for
details.

