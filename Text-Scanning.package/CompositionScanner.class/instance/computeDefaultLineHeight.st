scanning
computeDefaultLineHeight
	"Compute the default line height for a potentially empty text"
	rightMargin notNil
		ifTrue: [lastIndex := 1.
			self setFont.
			^ lineHeight + textStyle leading]
		ifFalse: [^textStyle lineGrid]