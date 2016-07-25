accessing
allFileNamesForVersionNamed: aString
	"avoid slow default implementation and directly check for existing files"
	|extensions|
	extensions := MCReader concreteSubclasses collect: [ :class| class extension ].
	^ extensions 
		collect: [ :extension| aString , '.', extension ]
		thenSelect: [ :fileName| self includesFileNamed: fileName ]