*Polymorph-Widgets
myDependents: aCollectionOrNil
	"Improved performance dependents."
	
	aCollectionOrNil 
		ifNil: [self removeProperty: #myDependents]
		ifNotNil: [self setProperty: #myDependents toValue: aCollectionOrNil]