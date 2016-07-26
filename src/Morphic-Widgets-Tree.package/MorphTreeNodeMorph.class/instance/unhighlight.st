updating
unhighlight
	complexContents highlightingColor
		ifNotNil: [
			(self valueOfProperty: #originalColor ifAbsent: [Color black])
				ifNotNil: [:c | self color: c]].
	self
		allMorphsDo: [:m | (m isKindOf: StringMorph)
				ifTrue: [m 
						color: (m
								valueOfProperty: #originalColor
								ifAbsent: [ Smalltalk ui theme textColor ])]].
