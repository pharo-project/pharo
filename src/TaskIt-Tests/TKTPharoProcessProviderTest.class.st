Class {
	#name : #TKTPharoProcessProviderTest,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #tests }
TKTPharoProcessProviderTest >> testNewProcessIsPharoProcess [
	self assert: ((TKTPharoProcessProvider new createProcessDoing: (MessageSend receiver: [ 1 + 1 ] selector: #value)) isKindOf: Process)
]
