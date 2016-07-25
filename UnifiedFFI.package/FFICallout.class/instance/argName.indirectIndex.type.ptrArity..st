spec parsing
argName: argName indirectIndex: anIndex type: typeName ptrArity: ptrArity
	| type |

	argName ifNil: [ 
		"allow nil,true,false as untyped arguments"
		^ self directArgName: typeName ptrArity: ptrArity ].

	type := self typeName: typeName pointerArity: ptrArity.
	type loader: (self loaderForArgNamed: argName indirectIndex: anIndex).
	
	^ type