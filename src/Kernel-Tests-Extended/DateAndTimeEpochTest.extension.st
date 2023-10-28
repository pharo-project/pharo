Extension { #name : 'DateAndTimeEpochTest' }

{ #category : '*Kernel-Tests-Extended' }
DateAndTimeEpochTest >> testAsMonth [
	self
		assert: aDateAndTime asMonth
		equals: (Month year: 1901 month: 'January')
]

{ #category : '*Kernel-Tests-Extended' }
DateAndTimeEpochTest >> testAsWeek [
	self assert: aDateAndTime asWeek equals: (Week starting: (Date readFrom: '31-12-1900' pattern: 'd-m-y'))
]

{ #category : '*Kernel-Tests-Extended' }
DateAndTimeEpochTest >> testAsYear [
	self assert: aDateAndTime asYear equals: (Year starting: (Date readFrom: '1-1-1901' pattern: 'd-m-y'))
]
