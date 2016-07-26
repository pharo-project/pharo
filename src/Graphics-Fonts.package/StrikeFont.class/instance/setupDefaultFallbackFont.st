multibyte character methods
setupDefaultFallbackFont
	| fonts f |
	fonts := TextStyle default fontArray.
	f := fonts first.
	1 
		to: fonts size
		do: [ :i | self height > (fonts at: i) height ifTrue: [ f := fonts at: i ] ].
	self fallbackFont: f.
	self reset