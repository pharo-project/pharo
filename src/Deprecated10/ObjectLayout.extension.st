Extension { #name : #ObjectLayout }

{ #category : #'*Deprecated10' }
ObjectLayout class >> kind [

	self
		deprecated: 'Should use mcTypeSymbol instead'
		on: '08 September 2021'
		in: #Pharo10
		transformWith: '`@rec kind' -> '`@rec mcTypeSymbol'.
	^ self mcType
]
