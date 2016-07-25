accessing-classes
classList
	| offset |
	classes isEmpty ifTrue: [ ^ classes ].
	offset := classes first allSuperclasses size.
	^ classes collect: [ :each | | ident |
		ident := String 
			new: 2 * (0 max: each allSuperclasses size - offset) 
			withAll: $ .
		each isAbstract
			ifFalse: [ ident , each name ]
			ifTrue: [ 
				ident asText , each name asText 
					addAttribute: TextEmphasis italic;
					yourself ] ].