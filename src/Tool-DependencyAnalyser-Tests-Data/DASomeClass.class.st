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
