comparing
compareClass
	| oldLayout newLayout bitLayoutNames |
	oldLayout := builder oldClass classLayout class name.
	newLayout := builder layoutDefinition layoutClass.
	
	oldLayout = newLayout ifTrue:[ ^#() ].
	bitLayoutNames := BitsLayout allSubclasses collect: #name.
	
	(bitLayoutNames includes: newLayout )
		ifTrue:[ ^ { ShBitLayoutChanged new builder: builder; yourself. } ].

	(bitLayoutNames includes: oldLayout )
		ifFalse:[ ^ #() ].

	(builder layoutDefinition slots ~= builder oldClass slots)	
		ifTrue:[ ^ { ShBitLayoutChanged new builder: builder; yourself. } ].

	(builder oldClass superclass allSlots isEmpty not)	
		ifTrue:[ ^ { ShBitLayoutChanged new builder: builder; yourself. } ]
		ifFalse:[ ^ #() ].
