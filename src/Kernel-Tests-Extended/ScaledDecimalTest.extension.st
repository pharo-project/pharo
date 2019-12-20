Extension { #name : #ScaledDecimalTest }

{ #category : #'*Kernel-Tests-Extended' }
ScaledDecimalTest >> testExactNthRoot [
	| eight thousandth tenth two |
	eight := 8.0s1.
	two := eight raisedTo: 1 / 3.
	self assert: two equals: 2.
	self assert: (two class = eight class and: [ two scale = eight scale ]).
	thousandth := 0.001s3.
	tenth := thousandth raisedTo: 1 / 3.
	self assert: tenth * 10 equals: 1.
	self assert: (tenth class = thousandth class and: [ tenth scale = thousandth scale ])
]

{ #category : #'*Kernel-Tests-Extended' }
ScaledDecimalTest >> testExactSqrt [
	| four hundredth tenth two |
	four := 4.0s1.
	two := four sqrt.
	self assert: two equals: 2.
	self assert: (two class = four class and: [ two scale = four scale ]).
	hundredth := 0.01s2.
	tenth := hundredth sqrt.
	self assert: tenth * 10 equals: 1.
	self assert: (tenth class = hundredth class and: [ tenth scale = hundredth scale ])
]

{ #category : #'*Kernel-Tests-Extended' }
ScaledDecimalTest >> testInexactNthRoot [
	| tenth cubicRoot3 fifthRootTenth three |
	three := 3.0s1.
	cubicRoot3 := three raisedTo: 1 / 3.
	self assert: cubicRoot3 isFloat.
	self deny: cubicRoot3 squared equals: 3.
	tenth := 0.10s2.
	fifthRootTenth := tenth raisedTo: 1 / 5.
	self assert: fifthRootTenth isFloat.
	self deny: fifthRootTenth squared equals: tenth
]

{ #category : #'*Kernel-Tests-Extended' }
ScaledDecimalTest >> testInexactSqrt [
	| tenth sqrt3 sqrtTenth three |
	three := 3.0s1.
	sqrt3 := three sqrt.
	self assert: sqrt3 isFloat.
	self deny: sqrt3 squared equals: 3.
	tenth := 0.10s2.
	sqrtTenth := tenth sqrt.
	self assert: sqrtTenth isFloat.
	self deny: sqrtTenth squared equals: tenth
]
