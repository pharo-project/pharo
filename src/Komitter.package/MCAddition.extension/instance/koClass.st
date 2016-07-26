*Komitter-UI
koClass
	| klass |
	klass := definition koClass.
	klass ifNil: [ ^ nil ].
	definition addOperation: self on: klass.
	self isClassPatch
		ifTrue: [ klass added: true ].
	^ klass