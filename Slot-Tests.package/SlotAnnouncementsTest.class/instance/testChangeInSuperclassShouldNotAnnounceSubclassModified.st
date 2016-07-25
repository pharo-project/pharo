tests
testChangeInSuperclassShouldNotAnnounceSubclassModified

	self subscribeOn: ClassModifiedClassDefinition.
	
	"Create a class and a subclass"
	aClass := self make: [ :builder | 
		builder 
			name: self aClassName;
			slots: #() ].

	anotherClass := self make: [ :builder | 
		builder 
			name: self anotherClassName;
			superclass: aClass ].

	self assert: self collectedAnnouncements size equals: 0.
	
	"Modify the superclass"
	aClass := self make: [ :builder | 
		builder 
			name: self aClassName;
			slots: #(a b c) ].

	self assert: self collectedAnnouncements size equals: 1.
	self assert: self collectedAnnouncements first newClassDefinition equals: aClass.