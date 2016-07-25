public
checkName: aFileName fixErrors: fixErrors
	"Check a string aFileName for validity as a file name. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is just to truncate the name to the maximum length for this platform. Subclasses can do any kind of checking and correction appropriate for their platform."
	
	| maxLength |
	aFileName size = 0 ifTrue: [self error: 'zero length file name'].
	maxLength := self maxFileNameLength.
	aFileName size > maxLength ifTrue: [
		fixErrors
			ifTrue: [^ aFileName contractTo: maxLength]
			ifFalse: [self error: 'file name is too long']].
	^ aFileName