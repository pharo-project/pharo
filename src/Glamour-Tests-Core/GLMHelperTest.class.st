Class {
	#name : #GLMHelperTest,
	#superclass : #TestCase,
	#category : #Glamour-Tests-Core
}

{ #category : #tests }
GLMHelperTest >> testAsGlamorousMultiValue [
	self assert: (GLMMultiValue with: #(1 2 3)) equals: #(1 2 3) asGlamorousMultiValue.
	self
		assert: 1 asGlamorousMultiValue , 2 asGlamorousMultiValue , 3 asGlamorousMultiValue
		equals: (GLMMultiValue withAll: #(1 2 3))
]

{ #category : #tests }
GLMHelperTest >> testGlamourValue [
	self assert: ([ :a | a + 2 ] glamourValue: 40) equals: 42.
	self assert: ([ :a | a + 2 ] glamourValue: 40 asGlamorousMultiValue) equals: 42.
	self assert: ([ 42 ] glamourValue: 40 asGlamorousMultiValue) equals: 42.
	self assert: ([ :a | a first + a last ] glamourValue: #(40 2)) equals: 42.
	self assert: ([ :a :b | a + b ] glamourValue: (GLMMultiValue withAll: #(40 2))) equals: 42.
	self assert: ([ :a | a + 2 ] glamourValue: (GLMMultiValue withAll: #(40 2))) equals: 42.
	self assert: ([ 42 ] glamourValue: (GLMMultiValue withAll: #(40 2))) equals: 42
]
