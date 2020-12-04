"
A simple class to test dependency analyzer
"
Class {
	#name : #DASomeClass,
	#superclass : #Object,
	#category : #'Tool-DependencyAnalyser-Tests-Data'
}

{ #category : #'tests - test data' }
DASomeClass >> method1 [
	self unimplementedMethod
]

{ #category : #'tests - test data' }
DASomeClass >> method2 [
	"a message send not in Kernel package"
	(1 to: 3) 
		collect: [ :i | i * 2 ] 
		as: (self class environment at: #Array) "avoid a direct reference to Array"
]

{ #category : #'tests - test data' }
DASomeClass >> method3 [
	"We refer a class indirectly in a block"
	^ [ EncoderForSistaV1 new ] value
]

{ #category : #'tests - test data' }
DASomeClass >> method4: x [
	"a message send not in Kernel package used in a block"
	[ x copyFrom: 1 to: 2 ] value
]
