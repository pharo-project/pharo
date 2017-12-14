"
I can compare an old class and a builder to detect if the layout kind changed and if there is need to propagate it.
"
Class {
	#name : #ShBitLayoutChangeDetector,
	#superclass : #ShAbstractChangeDetector,
	#category : 'Shift-Changes'
}

{ #category : #comparing }
ShBitLayoutChangeDetector >> compareClass [
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

	(builder oldClass superclass allSlots isNotEmpty)	
		ifTrue:[ ^ { ShBitLayoutChanged new builder: builder; yourself. } ]
		ifFalse:[ ^ #() ].

]
