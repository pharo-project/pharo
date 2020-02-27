Extension { #name : #Symbol }

{ #category : #'*SUnit-Core' }
Symbol >> asTestSelector [

	"Converts a method selector into a test selector in form of #testOriginalSelector"

	"#+ asTestSelector >>> #testPlus"
	"#+-<> asTestSelector >>>  #testPlusMinusLessThanGreaterThan"
	"#message asTestSelector  >>> #testMessage"
	"#mySelector asTestSelector >>> #testMySelector"
	"#at:putNext: asTestSelector >>> #testAtPutNext"

	self isBinary ifTrue: [ 
		^ (#test, (self flatCollect: [ :each | (((Character specialCharacterNames at: each) copyReplaceAll: '-' with: ' ') substrings collect: #capitalized) joinUsing: ''])) asSymbol ].
	
	^ (#test, (((self copyReplaceAll: ':' with: ' ') substrings collect: #capitalized) joinUsing: '')) asSymbol
]
