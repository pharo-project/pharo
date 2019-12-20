Extension { #name : #TimespanDoTest }

{ #category : #'*Kernel-Tests-Extended' }
TimespanDoTest >> testMonthsDo [
	| monthArray |
	monthArray := Array
		with: (Month starting: (DateAndTime year: 2003 day: 1) duration: 31 days)
		with: (Month starting: (DateAndTime year: 2003 day: 32) duration: 28 days)
		with: (Month starting: (DateAndTime year: 2003 day: 60) duration: 31 days)
		with: (Month starting: (DateAndTime year: 2003 day: 91) duration: 30 days).
	self assert: aTimespan months equals: monthArray
]

{ #category : #'*Kernel-Tests-Extended' }
TimespanDoTest >> testWeeksDo [
	| weekArray |
	weekArray := OrderedCollection new.
	7 to: 98 by: 7 do: [ :each | weekArray addLast: (Week starting: (DateAndTime year: 2003 day: each) duration: 7 days) ].
	weekArray := weekArray asArray.
	self assert: aTimespan weeks equals: weekArray
]

{ #category : #'*Kernel-Tests-Extended' }
TimespanDoTest >> testYearsDo [
	| yearArray |
	yearArray := Array with: (Year starting: (DateAndTime year: 2003 day: 7) duration: 365 days).
	self assert: aTimespan years contents equals: yearArray contents
]
