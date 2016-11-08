I represent someking of constant type arrays . 
My Main purpose is being used into structures.

For example, a structure like this: 

struct {
	int field[4];
}

can be modeled like this: 

TheStruct class>>initialize
	Int4 := FFITypeArray ofType: #int size: 4 

TheStruct class>>fieldsDesc 
	^ #(
	Int4 field;
	)

Of course this is not the best way to do it (parser needs to be adapted to do this automatically), but is a good and fast way to provide the functionality. 