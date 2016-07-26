update
updateTextPreview
	"Update the text preview."

	|str text|
	str := self openSelectedFile.
	str ifNil: [^self].
	[[text := str next: 5000]
		on: Error do: []]
		ensure: [str close].
	text ifNil: [text := ''].
	self previewMorph
		setText: text