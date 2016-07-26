update
updateImagePreview
	"Update the image preview."

	|str form|
	(self isImageFile: self selectedFileName)
		ifFalse: [^self previewMorph
					image: nil
					size: self previewSize].
	str := self openSelectedFile.
	str ifNil: [^self].
	[[str binary.
	form := ImageReadWriter formFromStream: str]
		on: Error do: []]
		ensure: [str close].
	self previewMorph
		image: form
		size: self previewSize