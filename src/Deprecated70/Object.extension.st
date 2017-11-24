Extension { #name : #Object }

{ #category : #'*Deprecated70' }
Object >> assert: aBlock descriptionBlock: descriptionBlock [
	
	self
		deprecated: 'Please use #assert:description: instead'
		transformWith: '`@receiver assert: `@statements1 descriptionBlock: `@statements2' 
						-> '`@receiver assert: `@statements1 description: `@statements2'.

	^ self assert: aBlock description: descriptionBlock
]
