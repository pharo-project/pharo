*opalcompiler-core
asMethodPreamble
	self numArgs = 0
		ifTrue: [ ^ self asString ].
	^ String
		streamContents: [ :str | 
			self keywords
				doWithIndex: [ :each :index | 
					str
						nextPutAll: each;
						nextPutAll: ' var';
						nextPutAll: index asString ] ]