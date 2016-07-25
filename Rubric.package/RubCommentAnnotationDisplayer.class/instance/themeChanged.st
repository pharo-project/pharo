event handling
themeChanged

	self color: self backgroundColor .
	
	self allMorphsDo: [ :morph | 
		(morph isKindOf: StringMorph) ifTrue: [ 
			morph color: self theme textColor	
		] 
	].

	super themeChanged