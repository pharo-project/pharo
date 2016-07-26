as yet unclassified
newContentMorph
	| top bottom |
	self textMorph: self newTextMorph.
	self text: self message.
	self iconMorph: self newIconMorph.
	top := self newRow: {self iconMorph. self textMorph}.
	bottom := super newContentMorph.
	^ self newGroupboxFor: (self newColumn: {top. bottom}).