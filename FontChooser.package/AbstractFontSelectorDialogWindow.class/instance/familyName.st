accessing
familyName
	"Answer the selected family name or nil if none."

	(self fontFamilyIndex between: 1 and: self fontFamilies size)
		ifFalse: [^nil].
	^(self fontFamilies at: self fontFamilyIndex) asString