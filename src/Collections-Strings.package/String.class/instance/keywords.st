converting
keywords
	"Answer an array of the keywords that compose the receiver."
	| keywords |
	keywords := Array streamContents:
		[:kwds | | char kwd | kwd := (String new: 16) writeStream.
		1 to: self size do:
			[:i |
			kwd nextPut: (char := self at: i).
			char = $: ifTrue: 
					[kwds nextPut: kwd contents.
					kwd reset]].
		(kwd position = 0) ifFalse: [kwds nextPut: kwd contents]].
	(keywords size >= 1 and: [(keywords at: 1) = ':']) ifTrue:
		["Has an initial keyword, as in #:if:then:else:"
		keywords := keywords allButFirst].
	(keywords size >= 2 and: [(keywords at: keywords size - 1) = ':']) ifTrue:
		["Has a final keyword, as in #nextPut::andCR"
		keywords := keywords copyReplaceFrom: keywords size - 1
								to: keywords size with: {':' , keywords last}].
	^ keywords