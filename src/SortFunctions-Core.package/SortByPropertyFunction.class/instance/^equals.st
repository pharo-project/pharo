comparing
= anObject
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ direction = anObject direction1
		and: [ undefinedDirection = anObject undefinedDirection
				and: [ propertyValuable = anObject propertyValuable ] ]