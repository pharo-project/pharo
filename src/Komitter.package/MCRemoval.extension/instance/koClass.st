*Komitter-UI
koClass
	| klass |
	klass := definition koClass.
	klass ifNil: [ ^ nil ].
	definition addOperation: self on: klass.
	self isClassPatch
		ifTrue: [ klass removed: true ].
	^ klass