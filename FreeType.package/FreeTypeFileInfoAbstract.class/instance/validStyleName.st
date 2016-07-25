accessing
validStyleName
	"answer the receiver's styleName, or an alternative 
	name to use if the styleName is invalid for some reason"
	
	
	(styleName copyWithout: $? ) 
		ifEmpty:[ | answer |
			"workaround problem with FreeType 2.2.1 and MS Gothic, MS Mincho
			where familyName is not read correctly. This may be fixed in later versions
			of FreeType"
			answer := ''.
			italic ifTrue:[answer := answer , 'Italic '].
			bold ifTrue:[answer := answer, 'Bold '].
			(italic or:[bold]) not ifTrue:[answer := answer, 'Regular '].
			^answer trimBoth].
	^styleName