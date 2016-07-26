converting
separateKeywords
	"#'foo:zork:' separateKeywords -> 'foo:  zork:'"

	self isKeyword
		ifFalse: [ ^ self ].
	^ String streamContents: [ :stream | 
			(self findTokens: $:) 
				do: [ :each | 
					stream
						nextPutAll: each;
						nextPut: $: ]
				separatedBy: [ stream nextPutAll: '  ' ] ]