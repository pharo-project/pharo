method
removeMethods: aCollection inClass: aClass
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If 
	so, remove it. "
	| messagesName confirmation |
	
	aCollection isEmptyOrNil ifTrue: [^ false].
	aCollection size = 1
		ifTrue: [ ^ self removeMethod: aCollection first inClass: aClass ].
	messagesName := aCollection collect: #selector.
	confirmation := self systemNavigation confirmRemovalOfSelectors: messagesName on: aClass.
	confirmation = 3
		ifTrue: [ ^ false ].
	aCollection do: [:message|| messageName |	
		messageName := message selector.
		(aClass includesLocalSelector: messageName)
			ifTrue: [ aClass removeSelector: messageName ]
			ifFalse: [ self removeNonLocalSelector: messageName ]].

	"In case organization not cached"
	confirmation = 2
		ifTrue: [ 
			aCollection do: [:message || messageName |	
				messageName := message selector.
				((self systemNavigation allCallsOn: messageName) notEmpty)
					ifTrue: [ self systemNavigation browseAllSendersOf: messageName ]]].
	^ true