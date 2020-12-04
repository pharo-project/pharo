"
SUnit tests for random generator
"
Class {
	#name : #RandomTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'gen'
	],
	#category : #'Random-Tests'
}

{ #category : #coverage }
RandomTest >> classToBeTested [
	
	^ Random
]

{ #category : #running }
RandomTest >> setUp [
	super setUp.
	gen := Random seed: 112629.
]

{ #category : #tests }
RandomTest >> testDistribution [
	| results occurrences generator |
	1 to: 100 do: [ :i |
		results := Bag new.
		generator := Random new seed: 1234567+i.
		2000 timesRepeat: [ results add: (100 atRandom: generator) ].
		occurrences := Array new: 100 streamContents: [ :out |
			results doWithOccurrences: [ :element :occurrence |
				self assert: (element between: 1 and: 100).
				out nextPut: occurrence.
				self assert: (occurrence between: 5 and: 40) ] ].
		self assert: occurrences average equals: 20.
		self assert: occurrences stdev < 10].
]

{ #category : #tests }
RandomTest >> testIfCompletelyBroken [
	"If the results are not these values (accounting for precision of printing) 
	then something is horribly wrong"

	gen seed: 2345678901.
	self
		assert: ((1 to: 10) collect: [ :i | gen next round: 15 ])
		equals:
			#(0.149243269650845 0.331633021743797 0.756196448000239 0.393701540023881 0.941783181364547 0.549929193942775 0.659962596213428 0.991354559078512 0.696074432551895 0.922987899707159)
]

{ #category : #tests }
RandomTest >> testNext [

	10000 timesRepeat: [ 
		| next | 
		next := gen next.
		self assert: (next >= 0).
		self assert: (next < 1).
	].
]

{ #category : #tests }
RandomTest >> testNextInt [
	| int |
	int := gen nextInt: 256.
	self assert: int isInteger.
	self assert: (int between: 1 and: 256)
]

{ #category : #tests }
RandomTest >> testNextInto [
	| array |
	array := Array new: 8.
	array := gen next: 8 into: array.
	self assert: (array allSatisfy: [ :each | each isFloat and: [ each >= 0 and: [ each < 1 ] ] ])
]

{ #category : #tests }
RandomTest >> testUnixRandomGeneratorSeed [
	"the test is very slow on the CI (>2 minutes)"
	self skipOnPharoCITestingEnvironment.
	gen useUnixRandomGeneratorSeed.
	100 timesRepeat: [ | next |
			next := gen next.
			self assert: next >= 0.
			self assert: next < 1 ]
]
