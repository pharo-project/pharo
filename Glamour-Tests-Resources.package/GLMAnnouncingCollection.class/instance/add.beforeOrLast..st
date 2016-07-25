initialization
add: newObject beforeOrLast: oldObject 
	(collection includes: oldObject)
		ifTrue: [ collection add: newObject before: oldObject ]
		ifFalse: [ collection add: newObject ].
	self announce: GLMItemAdded