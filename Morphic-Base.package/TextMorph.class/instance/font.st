accessing
font
	"Answer the probable font"

	self textStyle fonts ifEmpty: [^TextStyle defaultFont].
	^self textStyle defaultFont.
		