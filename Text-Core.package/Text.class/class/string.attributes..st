instance creation
string: aString attributes: atts
	"Answer an instance of me whose characters are those of aString.
	atts is an array of TextAttributes."

	^self string: aString runs: (RunArray new: aString size withAll: atts)