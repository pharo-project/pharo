transmitting
value
	<debuggingStone: #transmissionTransformation label: 'Transmission transformation'>
	| originalValue |
	originalValue := self originReferences size = 1 
		ifTrue: [ self originReferences first value ] 
		ifFalse: [ GLMMultiValue withAll: (self originReferences collect: [:each | each value]) ].
	^ self transformation isNil 
		ifTrue: [ originalValue ] 
		ifFalse: [ self transformation glamourValue: originalValue ]