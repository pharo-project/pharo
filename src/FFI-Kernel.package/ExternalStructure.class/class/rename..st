class management
rename: aString
	| oldName |
	oldName := name.
	super rename: aString.
	oldName = name ifFalse:[ExternalType noticeRenamingOf: self from: oldName to: name].