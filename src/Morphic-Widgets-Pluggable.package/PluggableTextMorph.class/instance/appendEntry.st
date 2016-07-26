settings
appendEntry
	"Append the text in the model's writeStream to the editable text. "
	textMorph asText size > model characterLimit ifTrue:
		["Knock off first half of text"
		self selectInvisiblyFrom: 1 to: textMorph asText size // 2.
		self replaceSelectionWith: Text new].
	self selectInvisiblyFrom: textMorph asText size + 1 to: textMorph asText size.
	self replaceSelectionWith: model contents asText.
	self selectInvisiblyFrom: textMorph asText size + 1 to: textMorph asText size