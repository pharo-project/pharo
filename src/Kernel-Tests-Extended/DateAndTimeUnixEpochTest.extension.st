Extension { #name : 'DateAndTimeUnixEpochTest' }

{ #category : '*Kernel-Tests-Extended' }
DateAndTimeUnixEpochTest >> testAsMonth [
	self
		assert: aDateAndTime asMonth
		equals: (Month year: 1970 month: 'January')
]

{ #category : '*Kernel-Tests-Extended' }
DateAndTimeUnixEpochTest >> testAsWeek [
	self assert: aDateAndTime asWeek equals: (Week starting: (Date readFrom: '31-12-1969' pattern: 'd-m-y'))
]

{ #category : '*Kernel-Tests-Extended' }
DateAndTimeUnixEpochTest >> testAsYear [
	self assert: aDateAndTime asYear equals: (Year starting: (Date readFrom: '1-1-1970' pattern: 'd-m-y'))
]
