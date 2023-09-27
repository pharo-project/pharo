Extension { #name : 'SequenceableCollection' }

{ #category : '*TestRunner-Core' }
SequenceableCollection >> runBy: aTestRunner [
	
	aTestRunner runSequenceableCollection: self
]
