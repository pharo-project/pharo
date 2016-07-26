dependencies
removeDependent: anObject
	"Remove the given object as one of the receiver's dependents."

	| dependents |
	dependents := self dependents reject: [:each | each == anObject].
	self myDependents: (dependents isEmpty ifFalse: [dependents]).
	^ anObject