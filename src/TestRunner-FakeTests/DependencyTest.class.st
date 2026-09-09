Class {
	#name : 'DependencyTest',
	#superclass : 'MockTest',
	#classInstVars : [
		'data'
	],
	#category : 'TestRunner-FakeTests-Mock Tests',
	#package : 'TestRunner-FakeTests',
	#tag : 'Mock Tests'
}

{ #category : 'initialization' }
DependencyTest class >> data [

	^ data
]

{ #category : 'initialization' }
DependencyTest class >> data: anInteger [

	data := anInteger
]

{ #category : 'initialization' }
DependencyTest class >> incrementData [
	data add: 1
]

{ #category : 'class initialization' }
DependencyTest class >> reset [

	<script>
	data := OrderedCollection new
]

{ #category : 'asserting' }
DependencyTest >> assertCountThenIncrement: anObject [

	self assert: self count equals: anObject .
	self class incrementData
]

{ #category : 'accessing' }
DependencyTest >> count [

	^ self class data size
]

{ #category : 'script' }
DependencyTest >> resetData [
	<script>
	self class reset
]

{ #category : 'tests' }
DependencyTest >> testDataIs10ThenIncrement [

	self 
		resetData;
		assertCountThenIncrement: 0
]

{ #category : 'tests' }
DependencyTest >> testDataIs11ThenIncrement [

	self assertCountThenIncrement: 1
]

{ #category : 'tests' }
DependencyTest >> testDataIs12ThenIncrement [

	self assertCountThenIncrement: 2
]

{ #category : 'tests' }
DependencyTest >> testDataIs13ThenIncrement [

	self assertCountThenIncrement: 3
]

{ #category : 'tests' }
DependencyTest >> testDataIs14ThenIncrement [

	self assertCountThenIncrement: 4
]

{ #category : 'tests' }
DependencyTest >> testDataIs15ThenIncrement [

	self assertCountThenIncrement: 5
]
