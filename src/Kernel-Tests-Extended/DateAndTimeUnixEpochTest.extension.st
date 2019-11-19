Extension { #name : #DateAndTimeUnixEpochTest }

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeUnixEpochTest >> testAsMonth [
	self 
		assert: aDateAndTime asMonth
		equals: (Month year: 1970 month: 'January'). 

]

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeUnixEpochTest >> testAsWeek [
	self assert: aDateAndTime asWeek equals: (Week starting: '12-31-1969' asDate)
]

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeUnixEpochTest >> testAsYear [
	self assert: aDateAndTime asYear equals: (Year starting: '01-01-1970' asDate)
]
