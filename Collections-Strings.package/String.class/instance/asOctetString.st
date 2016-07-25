converting
asOctetString
	"Convert the receiver into an octet string, if possible"
	"(IE, I am a WideString containing only character with codePoints < 255, so all of them fit in a latin1-string)."
	| string |
	string := String new: self size.
	1 to: self size do: [:i | string at: i put: (self at: i)].
	^string