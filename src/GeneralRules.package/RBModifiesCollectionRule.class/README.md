Checks for remove:''s of elements inside of collection iteration methods such as do:. These can cause the do: method to break since it will walk of the end of the collection. The common fix for this problem is to copy the collection before iterating over it.

For example turning

aCol do: [ :each |  ... aCol remove:... ]

into 

aCol copy do: [ :each |  ... aCol remove:... ]