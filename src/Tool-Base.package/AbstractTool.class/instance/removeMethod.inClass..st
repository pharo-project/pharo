method
removeMethod: aMethod inClass: aClass
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If 
	so, remove it. "
	| messageName confirmation |
	
	aMethod ifNil: [^ false].
	messageName := aMethod selector.
	confirmation := self systemNavigation confirmRemovalOf: messageName on: aClass.
	confirmation = 3
		ifTrue: [^ false].
	(aClass includesLocalSelector: messageName)
		ifTrue: [ aClass removeSelector: messageName ]
		ifFalse: [ self removeNonLocalSelector: messageName ].

	"In case organization not cached"
	confirmation = 2
		ifTrue: [self systemNavigation browseAllSendersOf: messageName].
		
	self removeEmptyUnclassifiedCategoryFrom: aClass.
	^ true