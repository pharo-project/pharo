Extension { #name : #Symbol }

{ #category : #'*opalcompiler-core' }
Symbol >> asMethodPreamble [
	self numArgs = 0
		ifTrue: [ ^ self asString ].
	^ String
		streamContents: [ :str | 
			| keywords |
			keywords := self keywords.
			keywords
				doWithIndex: [ :each :index | 
					str
						nextPutAll: each;
						nextPutAll: ' var';
						nextPutAll: index asString.
					index = keywords size ifFalse: [ str space ] ] ]
]

{ #category : #'*opalcompiler-core' }
Symbol >> asOneArgSelector [
	^ String streamContents: [:str|
		| parts |
		parts := (self findTokens: ':)').
			str nextPutAll: parts first.
			parts allButFirst do: [:each | str nextPutAll: each capitalized]	.
			str nextPutAll: ':']
]
