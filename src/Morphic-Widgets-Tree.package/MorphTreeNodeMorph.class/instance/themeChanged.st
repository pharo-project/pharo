updating
themeChanged
 
	self allMorphsDo: [ :m | (m isKindOf: StringMorph) ifTrue: [ m color: self theme textColor ] ].
	
	super themeChanged