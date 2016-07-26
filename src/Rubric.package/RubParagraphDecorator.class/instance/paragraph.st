instead of DNU
paragraph
	"this method is here to find the paragraph in the chain, instead of relying on implementing #doesNotUnderstand: !!!"

	| p |
	
	p := next.
	
	[ p  isNotNil and: [ p isKindOf: RubParagraph ] ] whileFalse: [ 
		p := p next.
	].

	^p
	
	