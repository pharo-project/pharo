accessing
instancesToTry
	^ { 
	Object new . 
	Class . 
	Metaclass . 
	Array new .
	Dictionary new . 
	WordArray new. 
	ByteArray new. 
	Color red .
	thisContext . 
	12 . 
	#symbol . 
	(12.32) . 
	TBehavior . 
	(SmallInteger>>#+) . 
	#(1 2 3) . 
	#[1 2 3] .
	[  1 + 2 ] .
	Error new .
	(1 << 40)
	}