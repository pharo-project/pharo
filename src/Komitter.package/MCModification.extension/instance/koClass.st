*Komitter-UI
koClass
	| klass |
	klass := obsoletion koClass.
	klass ifNil: [ ^ nil ].
	obsoletion addOperation: self on: klass.
	self isClassPatch
		ifTrue: [ klass modified: true ].
	^ klass