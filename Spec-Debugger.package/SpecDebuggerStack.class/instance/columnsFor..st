printing
columnsFor: anEntry
	"protocol: printing"

	"Answer the array of contexts."

	| method classLocation receiverClass |
	method := anEntry home method.
	classLocation := method methodClass.
	receiverClass := anEntry receiver class.

	"If the receiver is differen from the implementor, show the original method implementor"
	classLocation := classLocation = receiverClass
		ifTrue: [ classLocation name ]
		ifFalse: [ receiverClass name , '(' , classLocation name , ')' ].
	anEntry isBlockContext
		ifTrue: [ classLocation := '[...] in ' , classLocation ].
	^ {classLocation.
	method selector asString}