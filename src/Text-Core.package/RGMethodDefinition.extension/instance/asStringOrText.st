*Text-Core
asStringOrText

	| text |
	self realClass ifNil: [ ^self fullName ].
	text := (self fullName,' {', (self protocol ifNil: ['']), '} ') asText.
	self isLocalSelector
		ifFalse: [ text addAttribute: TextEmphasis italic ].
	^ text