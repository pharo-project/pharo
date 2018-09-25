Extension { #name : #DateAndTimeLeapTest }

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeLeapTest >> testAsMonth [
	self
		assert: aDateAndTime asMonth
		equals: ((Month year: 2004 month: 'February') translateTo: 2 hours).

]

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeLeapTest >> testAsWeek [
	self assert: aDateAndTime asWeek equals: ((Week starting: '02-29-2004' asDate) translateTo: 2 hours).

]

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeLeapTest >> testAsYear [
	"A year always starts at January 1"
	self 
		assert: aDateAndTime asYear
		equals: ((Year starting: '02-29-2004' asDate) translateTo: 2 hours ).  
	self
		assert: aDateAndTime asYear
		equals: ((Year starting: '01-01-2004' asDate) translateTo: 2 hours)  

]
