Extension { #name : 'SequenceableCollection' }

{ #category : '*TestRunner-Core' }
SequenceableCollection >> runBy: aTestRunner [
	
	aTestRunner runSequenceableCollection: self
]

{ #category : '*TestRunner-Core' }
SequenceableCollection >> suite [ 

	^ TestSuite new addTests: self; yourself
]
