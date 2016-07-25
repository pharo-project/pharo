I'm an offset accessor for nested structures. 
The idea is that I remap the accessing to an offseted byte array, thus providing access to the inner structure without copying the values. 
This is useful when you have nested structures, something like this: 

struct StructNested {
	int value;
}

struct StructA {
	int one;
	struct StructNested theNest;
}

then you can do something like this: 

s := StructA new.
s theNest value: 42. 

And the value will be set in the StructA instance, not in a copy as before. 