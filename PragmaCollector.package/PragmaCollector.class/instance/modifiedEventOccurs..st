system changes
modifiedEventOccurs: anEvent
	"
	a method has been updated: try to update an handler:
	1 - I already have one for the method, then
	do nothing, 
	2 - I do not have one but method has an acceptable
	pragma in it, then I try to add a new handler
	3 - I have one but changed method has no more
	acceptable pragma in it, then the handler is removed.
	"

	Pragma
		withPragmasIn: anEvent methodClass
		do: [ :pragma | 
			pragma methodSelector = anEvent selector
				ifTrue: [ (self
						detect: [ :oldprag | 
							oldprag methodSelector = pragma methodSelector
								and: [ oldprag methodClass = anEvent methodClass ] ])
						ifNotNil: [ :oldprag | ^ self updatePragma: oldprag ].
					^ self addPragma: pragma ] ].
	"No pragma but an handler for the method"
	(self
		pragmaWithSelector: anEvent selector
		inClass: anEvent methodClass)
		ifNotNil: [ :found | self removePragma: found ]