tests
testChangeInSharedVariablesShouldAnnounceClassModified

	self subscribeOn: ClassModifiedClassDefinition.
	
	"Create a class"
	aClass := self make: [ :builder | 
		builder 
			name: self aClassName ].

	self assert: self collectedAnnouncements size equals: 0.
	
	"Modify the class"
	aClass := self make: [ :builder | 
		builder 
			name: self aClassName;
			sharedVariablesFromString: 'A B C' ].

	self assert: self collectedAnnouncements size equals: 1.
	self assert: self collectedAnnouncements first newClassDefinition equals: aClass.