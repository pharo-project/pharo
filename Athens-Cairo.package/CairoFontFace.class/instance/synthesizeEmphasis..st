accessing
synthesizeEmphasis: emphasisCode
	emphasisCode = (TextEmphasis bold emphasisCode | TextEmphasis italic emphasisCode)
		ifTrue: [ ^ self synthesizeBoldAndOblique ].
	emphasisCode = TextEmphasis bold emphasisCode
		ifTrue: [ ^ self synthesizeBold ].
	emphasisCode = TextEmphasis italic emphasisCode
		ifTrue: [ ^ self synthesizeOblique ]