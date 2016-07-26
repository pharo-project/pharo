accessing
asString
	"[FAMILY-LIST] [STYLE-OPTIONS] [SIZE]"
	| str |
	str := '' writeStream.
	str nextPutAll:  family; space.
	"	
2)STYLE_OPTIONS is a whitespace separated list of words where each WORD describes one of style, variant, weight, stretch, or gravity. If STYLE-OPTIONS is missing, then all style options will be set to the default values
"
	#(slant. variant. weight. stretch. gravity) do:[:each|
		|val|
		val := options at: each ifAbsent: nil.
		val notNil ifTrue:[str nextPutAll: val ; space ]].
	
	self fontSize notNil ifTrue:[
		str
			space;
			nextPutAll: self fontSize asString ].
	^ str contents