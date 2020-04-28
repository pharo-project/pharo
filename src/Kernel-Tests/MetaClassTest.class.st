Class {
	#name : #MetaClassTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #tests }
MetaClassTest >> testHasBindingThatBeginsWith [
	self assert: (SmalltalkImage class hasBindingThatBeginsWith: 'Compiler').
	self deny: (SmalltalkImage class hasBindingThatBeginsWith: 'Object').
	
	"Pools are looked up, too"
	self assert: (TimeZone class hasBindingThatBeginsWith: 'DaysInMo')
]
