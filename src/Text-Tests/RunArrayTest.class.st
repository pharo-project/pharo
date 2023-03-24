"
SUnit tests for class RunArray
"
Class {
	#name : #RunArrayTest,
	#superclass : #TestCase,
	#traits : 'TSequenceableTruncatedToSize',
	#classTraits : 'TSequenceableTruncatedToSize classTrait',
	#instVars : [
		'collectionWith5Elements'
	],
	#category : #'Text-Tests-Base'
}

{ #category : #'*Text-Tests' }
RunArrayTest >> testScanFromANSICompatibility [

	RunArray scanFrom: '()f1dNumber new;;' readStream.
	RunArray scanFrom: '()a1death;;' readStream.
	RunArray scanFrom: '()F1death;;' readStream
]
