parsing
parse

	| styleName |
	styleNameIn := self splitBadTokensIn: styleNameIn.
	combinedName := styleNameIn trimBoth.
	tokens := self tokenize: combinedName.
	self extractUpright.
	styleName := combinedName.
	combinedName := familyNameIn trimBoth.
	self addStyleNameToCombinedName: styleName..
	tokens := self tokenize: combinedName.
	self extractSlant.
	tokens := self tokenize: combinedName.
	self extractStretch.
	tokens := self tokenize: combinedName.
	self extractWeight.
	