option
shouldExtractAssignmentTo: aString
	^ self confirm: ('Do you want to extract the assignment of <1s> at the end of selection?' expandMacrosWith: aString)