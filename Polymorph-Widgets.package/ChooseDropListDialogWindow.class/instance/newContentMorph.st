as yet unclassified
newContentMorph
	"Answer a new content morph."

	self iconMorph: self newIconMorph.
	self textMorph: self newTextMorph.
	self listMorph: self newListMorph.
	^self newGroupboxForAll: {
		self newRow: {self iconMorph. self textMorph}.
		self listMorph}