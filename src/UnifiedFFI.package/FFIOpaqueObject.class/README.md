I represent an opaque type (https://en.wikipedia.org/wiki/Opaque_data_type) living in the C heap. 
This is usefull when working with obscure types (types we do not know its composition).
Take this as an example (taken from SQLite bindings): 

[ [ [ 
typedef struct sqlite3 sqlite3;
typedef struct sqlite3_stmt sqlite3_stmt;
 ] ] ]

this defines a type of a structure, and then is used along the api in this way: 

int sqlite3_open(char *filename, sqlite3 **handle);
char *sqlite3_column_name(sqlite3_stmt* aStatement, int aColumn)

this means we always access them through a reference. Now, we could declare the pointer to the structures, but then our declarations wouldn't be as close to C as we want. 

This i where I come to help. Instead using a FFIExternalObject, we use an FFIOpaqueObject. This means we will have a type reference (that we cannot use dereferenced, see ==FFIDereferencedOpaqueObjectError==).

In the case of the example, it would work like this: 

[ [ [ 
TypeMap := Dictionary newFromPairs: #(
	sqlite3		      	FFIOpaqueObject
	sqlite3_stmt		FFIOpaqueObject
).

self ffiCall: #(int sqlite3_open(String filename, sqlite3 **handle)).
...
 self ffiCall: #(const void *sqlite3_column_blob (sqlite3_stmt* aStatement, int aColumn))
 ] ] ]
