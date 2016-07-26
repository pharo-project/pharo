initialize
refreshClasses: pack
	self resetClassSelection.
	pack 
		ifNil: [ classes items: #() ] 
		ifNotNil: [ classes items: (self classesOfPackage: pack)]. 
	Transcript show: pack printString;cr