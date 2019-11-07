Extension { #name : #Symbol }

{ #category : #'*SUnit-Core' }
Symbol >> asTestSelector [

	self isBinary ifTrue: [ 
		^ (#test, (self flatCollect: [ :each | (((Character specialCharacterNames at: each) copyReplaceAll: '-' with: ' ') substrings collect: #capitalized) joinUsing: ''])) asSymbol ].
	
	^ (#test, (((self copyReplaceAll: ':' with: ' ') substrings collect: #capitalized) joinUsing: '')) asSymbol
]
