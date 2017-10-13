*Tools
inspectSubInstances 
	"Inspect all instances of the receiver and all its subclasses.  CAUTION - don't do this for something as generic as Object!"

	| all allSize prefix |
	all := self allSubInstances.
	(allSize := all size) isZero ifTrue: [^ self inform: 'There are no 
instances of ', self name, '
or any of its subclasses'].
	prefix := allSize = 1
		ifTrue: 	['The lone instance']
		ifFalse:	['The ', allSize printString, ' instances'].
	
	all asArray inspectWithLabel: (prefix, ' of ', self name, ' & its subclasses')