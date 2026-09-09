Class {
	#name : 'StoreOnTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-Numbers',
	#package : 'Kernel-Tests',
	#tag : 'Numbers'
}

{ #category : 'tests' }
StoreOnTest >> testStoreOn [
	| float |
	float := -1.2.
	self assert: (String streamContents: [ :s | float storeOn: s ]) equals: ' -1.2'.

	float := 1.2.
	self assert: (String streamContents: [ :s | float storeOn: s ]) equals: '1.2'
]

{ #category : 'tests' }
StoreOnTest >> testStoreOnRoundTrip [
	"If storeOn: prints exactly and the parser avoid cumulating round off Errors,
	then Float should be read back exactly.
	Note: there is no guarantee to restore the bit pattern of NaN though"

	self assert: (OpalEvaluator new evaluate: Float halfPi storeString) equals: Float halfPi.
	self assert: (OpalEvaluator new evaluate: Float halfPi negated storeString) equals: Float halfPi negated.
	self assert: (OpalEvaluator new evaluate: Float infinity storeString) equals: Float infinity.
	self assert: (OpalEvaluator new evaluate: Float infinity negated storeString) equals: Float infinity negated.
	self assert: (OpalEvaluator new evaluate: Float nan storeString) isNaN
]
