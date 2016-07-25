class
browseUnusedMethodsOf: aClass
	| classes unsent messageList |
	
	aClass
		ifNil: [^ self].
		
	classes := Array with: aClass with: aClass class.
	unsent := Set new.
	classes
		do: [:c | unsent addAll: c selectors ].
	unsent := self systemNavigation allUnsentMessagesIn: unsent.
	messageList := OrderedCollection new.
	classes
		do: [:c | (c selectors
				select: [:s | unsent includes: s]) asSortedCollection
				do: [:sel | messageList add: ((c compiledMethodAt: sel) methodReference) ]].
	self systemNavigation browseMessageList: messageList name: 'Unsent Methods in ' , aClass name