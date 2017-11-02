Extension { #name : #CombinedChar }

{ #category : #'*Unicode-Initialization' }
CombinedChar class >> loadCompositionMapping [
	| url |
	url := 'http://unicode.org/Public/UNIDATA/UnicodeData.txt' asZnUrl.
	self parseCompositionMappingFrom: url retrieveContents readStream
]
