instance creation
fromTraditionalHeader: aString
	"This is a traditional non-MIME header (like Subject:) and so should be stored whole"

	| newValue |

	newValue := self new.
	newValue mainValue: aString.
	newValue parameters: #().
	^newValue.
