actions
newContentMorph
	"Answer a new content morph."

	self iconMorph: self newIconMorph.
	self textMorph: self newTextMorph.
	self textMorph wrapFlag: false.
	self textEditorMorph: self newTextEditorMorph.
	^self newGroupboxForAll: {
		self newRow: {self iconMorph. self textMorph}.
		self textEditorMorph}