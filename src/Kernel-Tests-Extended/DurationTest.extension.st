Extension { #name : #DurationTest }

{ #category : #'*Kernel-Tests-Extended' }
DurationTest >> testAsDays [
	self assert: (Duration days: 2) asDays equals: 2.
	self assert: (Duration weeks: 1) asDays equals: 7.
	self assert: (aDuration asDays closeTo: 1.08546)
]

{ #category : #'*Kernel-Tests-Extended' }
DurationTest >> testAsHour [
	| full half quarter |
	full := Duration minutes: 60.
	half := Duration minutes: 30.
	quarter := Duration minutes: 15.
	self
		assert: 1 hour equals: full;
		assert: 1.0 hour equals: full;
		assert: 0.5 hour equals: half;
		assert: (1 / 2) hour equals: half;
		assert: (1 / 4) hour equals: quarter
]

{ #category : #'*Kernel-Tests-Extended' }
DurationTest >> testAsHours [
	self assert: (Duration hours: 2) asHours equals: 2.
	self assert: (Duration days: 1) asHours equals: 24.
	self assert: (aDuration asHours closeTo: 26.0511)
]

{ #category : #'*Kernel-Tests-Extended' }
DurationTest >> testAsMinutes [
	self assert: (Duration seconds: 60) asMinutes equals: 1.
	self assert: (Duration hours: 1) asMinutes equals: 60.
	self assert: (aDuration asMinutes closeTo: 1563.0666).
	self assert: ((Duration milliSeconds: 100) asMinutes closeTo: 1 / 600)
]

{ #category : #'*Kernel-Tests-Extended' }
DurationTest >> testMonthDurations [
	| jan feb dec |
	jan := Duration month: #January.
	feb := Duration month: #February.
	dec := Duration month: #December.

	self
		assert: jan equals: Year current months first duration;
		assert: feb equals: Year current months second duration;
		assert: dec equals: Year current months last duration
]
