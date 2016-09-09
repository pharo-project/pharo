*opalcompiler-core
asMethodPreamble
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