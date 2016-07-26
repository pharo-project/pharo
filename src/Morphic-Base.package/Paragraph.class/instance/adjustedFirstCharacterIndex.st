access
adjustedFirstCharacterIndex
	"Return the index in the text where this paragraph WOULD begin if nothing had changed, except the size of the text -- ie if there have only been an insertion of deletion in the preceding morphs"
	offsetToEnd ifNil: [^ -1].
	^ text size - offsetToEnd