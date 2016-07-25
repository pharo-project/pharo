dependents
removeDependent: anObject
	"Remove the given object as one of the receiver's dependents."

	| newDependents |
	newDependents := self dependents reject: [:each | each == anObject].
	self myDependents: (newDependents isEmpty ifFalse: [ newDependents ]).
	
	^ anObject