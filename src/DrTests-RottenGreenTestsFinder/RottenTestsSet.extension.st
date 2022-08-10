Extension { #name : #RottenTestsSet }

{ #category : #'*DrTests-RottenGreenTestsFinder' }
RottenTestsSet >> addToTestRunCount: anInteger [

	testsRunCount := self testsRunCount + anInteger
]

{ #category : #'*DrTests-RottenGreenTestsFinder' }
RottenTestsSet >> addToTestsVisitedCount: anInteger [

	testsVisitedCount := self testsVisitedCount + anInteger
]

{ #category : #'*DrTests-RottenGreenTestsFinder' }
RottenTestsSet >> mergeWith: aRottenTestsSet [

	self
		addAll: aRottenTestsSet rottenTests;
		addToTestsVisitedCount: aRottenTestsSet testsVisitedCount;
		addToTestRunCount: aRottenTestsSet testsRunCount
]

{ #category : #'*DrTests-RottenGreenTestsFinder' }
RottenTestsSet >> testsRunCount [

	^ testsRunCount ifNil: [ testsRunCount := 0 ]
]

{ #category : #'*DrTests-RottenGreenTestsFinder' }
RottenTestsSet >> testsVisitedCount [

	^ testsVisitedCount ifNil: [ testsVisitedCount := 0 ]
]
