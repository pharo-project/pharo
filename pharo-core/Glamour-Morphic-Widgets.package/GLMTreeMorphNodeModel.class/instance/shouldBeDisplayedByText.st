testing
shouldBeDisplayedByText
	self containerTree shouldFilterByTextInput ifFalse: [ ^ true ].
	self withContentsDo: 
		[ :node | 
		self flag: 'this should be delegated to the presentation
		and glamourValue should be used to include the entity ports'.
		(self containerTree glamourPresentation filterStrategy 
			value: self containerTree inputText asString
			value: node item) == true ifTrue: [ ^ true ] ].
	^ false