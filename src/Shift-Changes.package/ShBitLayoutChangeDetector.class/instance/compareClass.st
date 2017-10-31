comparing
compareClass
	| oldLayout newLayout bitLayoutNames |
	oldLayout := builder oldClass classLayout class name.
	newLayout := builder layoutDefinition layoutClass name.
	
	oldLayout = newLayout ifTrue:[ ^#() ].
	bitLayoutNames := BitsLayout allSubclasses collect: [:e | e name].
	
	(bitLayoutNames includes: newLayout )
		ifTrue:[ ^ { ShBitLayoutChanged new builder: builder; yourself. } ].

	(bitLayoutNames includes: oldLayout )
		ifFalse:[ ^ #() ].

	(builder layoutDefinition slots size ~= builder oldClass slots size)	
		ifTrue:[ ^ { ShBitLayoutChanged new builder: builder; yourself. } ].

	(builder oldClass superclass allSlots isEmpty not)	
		ifTrue:[ ^ { ShBitLayoutChanged new builder: builder; yourself. } ]
		ifFalse:[ ^ #() ].
