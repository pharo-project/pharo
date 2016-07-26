accessing
interimContents: aString
	"The receiver is under edit and aString represents the string the user sees as she edits, which typically will not have been accepted and indeed may be abandoned"

	self contents: aString