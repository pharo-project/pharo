accessing class hierarchy
subclasses
	"Answer a Set containing the receiver's subclasses."

	^subclasses 
		ifNil: [ #() ]
		ifNotNil: [ subclasses copy ]