Extension { #name : #DurationTest }

{ #category : #'*Kernel-Tests-Extended' }
DurationTest >> testAsDays [
	self assert: (Duration days: 2) asDays = 2.
	self assert: (Duration weeks: 1) asDays = 7.	
	self assert: (aDuration asDays closeTo: 1.08546).
]

{ #category : #'*Kernel-Tests-Extended' }
DurationTest >> testAsHour [
	|full half quarter|
	full := (Duration minutes: 60).
	half := (Duration minutes: 30).
	quarter := (Duration minutes: 15).	
	self 
		assert: 1 hour = full;
		assert: 1.0 hour = full;
		assert: 0.5 hour = half; 
		assert: (1/2) hour = half;
		assert: (1/4) hour = quarter
]

{ #category : #'*Kernel-Tests-Extended' }
DurationTest >> testAsMinutes [
	self assert: (Duration seconds: 60) asMinutes = 1.
	self assert: (Duration hours: 1) asMinutes = 60.	
	self assert: (aDuration asMinutes closeTo:  1563.0666).
	self assert: ((Duration milliSeconds: 100) asMinutes closeTo: (1/600))
]

{ #category : #'*Kernel-Tests-Extended' }
DurationTest >> testMonthDurations [

	| jan feb dec |
	jan := Duration month: #January.
	feb := Duration month: #February.
	dec := Duration month: #December.
	
	self 
		assert: jan = (Year current months first duration);
		assert: feb = (Year current months second duration);
		assert: dec = (Year current months last duration)

		

]
