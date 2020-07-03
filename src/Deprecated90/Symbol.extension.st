Extension { #name : #Symbol }

{ #category : #'*Deprecated90' }
Symbol >> asIcon [
	self deprecated: 'Do not use #foo asIcon but self iconNamed: #foo' 
		transformWith: '`@rec asIcon'  -> 'self iconNamed: `@rec'.
	^ Smalltalk ui icons iconNamed: self
]
