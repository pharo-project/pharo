morphic ui
workingCopySearchAdd: aString 
	self workingCopySearchList: (self workingCopySearchList
			remove: aString
			ifAbsent: [];
			 yourself).
	self workingCopySearchList size = self workingCopySearchMaxSize
		ifTrue: [self workingCopySearchList removeLast].
	self workingCopySearchList addFirst: aString 