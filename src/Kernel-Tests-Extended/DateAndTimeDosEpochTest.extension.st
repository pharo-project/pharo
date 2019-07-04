Extension { #name : #DateAndTimeDosEpochTest }

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeDosEpochTest >> testAsMonth [
	self
		assert: aDateAndTime asMonth 
		equals: (Month year: 1980 month: 'January')

]

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeDosEpochTest >> testAsWeek [
	self assert: aDateAndTime asWeek equals: (Week starting: '12-31-1979' asDate) 


]

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeDosEpochTest >> testAsYear [
	self assert: aDateAndTime asYear equals: (Year starting: '01-01-1980' asDate) 

]
