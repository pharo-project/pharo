system changes
methodModified: anEvent

   "trait methods aren't handled here"
	anEvent isProvidedByATrait
		ifTrue: [ ^ self ].
		
	^ self
		managersForClass: anEvent methodClass
		selector: anEvent selector
		do: [ :mgr | mgr modified: true ]