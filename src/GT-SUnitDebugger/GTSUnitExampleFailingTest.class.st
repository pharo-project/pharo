"
I am not a test! I provide usage examples for the SUnit debugger.
"
Class {
	#name : #GTSUnitExampleFailingTest,
	#superclass : #TestCase,
	#category : #'GT-SUnitDebugger'
}

{ #category : #testing }
GTSUnitExampleFailingTest class >> isAbstract [
	^ true
]

{ #category : #testing }
GTSUnitExampleFailingTest class >> isUnitTest [
	^ false
]

{ #category : #tests }
GTSUnitExampleFailingTest >> checkWithFailure [
	self assert: false description: 'A failure'
]

{ #category : #tests }
GTSUnitExampleFailingTest >> intermediaryCheckWithFailure [
	40 + 2.
	self checkWithFailure
]

{ #category : #tests }
GTSUnitExampleFailingTest >> testArrays [
	 self
           assert: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 
49 50 51 51 51 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 
75 76 77 78 79 80 81 81 81 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99)
           equals: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 
23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 
49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 
75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99)
]

{ #category : #tests }
GTSUnitExampleFailingTest >> testDictionary [
	 self
		assert: (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 
49 50 51 51 51 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 
75 76 77 78 79 80 81 81 81 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101) inject: Dictionary new into: [ :dict :nr| dict at: nr put: nr<<5. dict ])
      equals: (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 
23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 
49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 
75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101) inject: Dictionary new into: [ :dict :nr| 
		nr\\40 == 0 
			ifTrue: [ dict at: nr put: nr<<6 ]
			ifFalse: [ dict at: nr put: nr<<5 ].
			dict ])
]

{ #category : #tests }
GTSUnitExampleFailingTest >> testFailingAssestionThroughAHelperMethod [

	self intermediaryCheckWithFailure
]

{ #category : #tests }
GTSUnitExampleFailingTest >> testFailingDate [
	|aDateAndTime|

	aDateAndTime :=  DateAndTime localTimeZone: TimeZone default; dosEpoch.
	self 
		assert: (aDateAndTime offset: '0:12:00:00')  
		equals: '1 January 1980 1:00 pm' asDateAndTime
]

{ #category : #tests }
GTSUnitExampleFailingTest >> testFailingText [

	self assert: 'I like pie  
pizza pie 
apple pie 
rad squared pi 
and other PIes'
	equals: 'I like pie  
pizza pie 
potato pie 
rad squared pi 
and other PIes'
]

{ #category : #tests }
GTSUnitExampleFailingTest >> testMultiValuedStreaming [
	| headers keysAndValues |
	headers := ZnHeaders new.
	headers at: 'Set-Cookie' put: 'foo=1'.
	headers at: 'Set-Cookie' add: 'bar=1'.
	headers at: 'Content-TYPE' put: 'text/plain'.
	headers at: 'Content-Length' put: '128'.
	keysAndValues := OrderedCollection streamContents: [ : stream |
		headers headersDo: [ :key :value |
			stream nextPut: key; nextPut: value ] ].
	self assert: keysAndValues equals: #('Set-Cookie' 'foo=1' 'Set-Cookie' 'bar=1' 'Content-Length' '128' 'Content-TYPE' 'text/plain') asOrderedCollection 
	
	
]

{ #category : #tests }
GTSUnitExampleFailingTest >> testSimpleFailingAssestion [

	self assert: false
]

{ #category : #tests }
GTSUnitExampleFailingTest >> testWithHalt [

	self assert: true.
	self halt
]
