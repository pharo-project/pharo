find-replace
openFindDialog
	self sharesFindReplace 
		ifTrue: [ (self findReplaceDialog on: self) open]
		ifFalse: [self flash]