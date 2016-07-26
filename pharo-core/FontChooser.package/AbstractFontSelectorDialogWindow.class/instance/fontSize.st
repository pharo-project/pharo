font description
fontSize
	"Answer the selected font size or nil if none."

	(self fontSizeIndex between: 1 and: self fontSizes size)
		ifFalse: [^nil].
	^self fontSizes at: self fontSizeIndex