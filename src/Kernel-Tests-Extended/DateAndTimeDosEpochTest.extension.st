Extension { #name : 'DateAndTimeDosEpochTest' }

{ #category : '*Kernel-Tests-Extended' }
DateAndTimeDosEpochTest >> testAsMonth [
	self
		assert: aDateAndTime asMonth
		equals: (Month year: 1980 month: 'January')
]

{ #category : '*Kernel-Tests-Extended' }
DateAndTimeDosEpochTest >> testAsWeek [
	self assert: aDateAndTime asWeek equals: (Week starting: (Date readFrom: '31-12-1979' pattern: 'd-m-y'))
]

{ #category : '*Kernel-Tests-Extended' }
DateAndTimeDosEpochTest >> testAsYear [
	self assert: aDateAndTime asYear equals: (Year starting: (Date readFrom: '1-1-1980' pattern: 'd-m-y'))
]
