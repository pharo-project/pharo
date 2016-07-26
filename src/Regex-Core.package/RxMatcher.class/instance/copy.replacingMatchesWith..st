match enumeration
copy: aString replacingMatchesWith: replacementString
	"Copy <aString>, except for the matches. Replace each match with <aString>."

	| answer |
	answer := (String new: 40) writeStream.
	self
		copyStream: aString readStream
		to: answer
		replacingMatchesWith: replacementString.
	^answer contents