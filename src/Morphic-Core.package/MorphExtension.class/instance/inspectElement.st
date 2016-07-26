other
inspectElement
	"Create and schedule an Inspector on the otherProperties and the 
	named properties."
	| key obj |
	key := UIManager default 
				chooseFrom: self sortedPropertyNames 
				values: self sortedPropertyNames
				title: 'Inspect which property?' translated.
	key
		ifNil: [^ self].
	obj := otherProperties
				at: key
				ifAbsent: ['nOT a vALuE'].
	obj = 'nOT a vALuE'
		ifTrue: [(self perform: key) inspect
			"named properties"]
		ifFalse: [obj inspect]