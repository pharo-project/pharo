testing
isSymbolFont
	| charmaps |
	symbolFont ifNotNil:[^symbolFont].
	self face isValid ifFalse:[^false].
	charmaps := self face charmaps.
	(charmaps includes: 'symb') ifTrue:[^symbolFont := true]."MS Symbol font"
	^symbolFont := false