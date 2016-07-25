class
removeClass: aClass
	"Remove the selected class from the system, at interactive user request.  Make certain the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| message className classToRemove result |
	aClass ifNil: [ ^ false ].
	classToRemove := aClass theNonMetaClass.
	className := classToRemove name.
	message := self removeClassMessageFor: className.
	(result := self confirm: message)
		ifTrue: [ 
			classToRemove subclasses notEmpty
				ifTrue: [ 
					(self confirm: 'class has subclasses: ' , message)
						ifFalse: [ ^ false ] ].
			classToRemove removeFromSystem ].
	^ result