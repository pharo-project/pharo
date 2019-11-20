Extension { #name : #DateAndTimeEpochTest }

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeEpochTest >> testAsMonth [
	self 
		assert: aDateAndTime asMonth
		equals: (Month year: 1901 month: 'January'). 

]

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeEpochTest >> testAsWeek [
	self assert: aDateAndTime asWeek equals: (Week starting: '12-31-1900' asDate)
]

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeEpochTest >> testAsYear [
	self assert: aDateAndTime asYear equals: (Year starting: '01-01-1901' asDate)
]
