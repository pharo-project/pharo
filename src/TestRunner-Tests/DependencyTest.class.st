Class {
	#name : 'DependencyTest',
	#superclass : 'MockTest',
	#classInstVars : [
		'data'
	],
	#category : 'TestRunner-Tests-Utilities-Test',
	#package : 'TestRunner-Tests',
	#tag : 'Utilities-Test'
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
	data := data + 1
]

{ #category : 'initialization' }
DependencyTest class >> reset [

	<script>
	data := 10
]

{ #category : 'script' }
DependencyTest >> resetData [
	<script>
	self class reset
]

{ #category : 'initialization' }
DependencyTest >> testDataIs10ThenIncrement [

	self skipOnPharoCITestingEnvironment.
	self resetData.
	self assert: self class data equals: 10.
	self class incrementData
]

{ #category : 'initialization' }
DependencyTest >> testDataIs11ThenIncrement [

	self skipOnPharoCITestingEnvironment.
	self assert: self class data equals: 11.
	self class incrementData
]

{ #category : 'initialization' }
DependencyTest >> testDataIs12ThenIncrement [

	self skipOnPharoCITestingEnvironment.
	self assert: self class data equals: 12.
	self class incrementData
]

{ #category : 'initialization' }
DependencyTest >> testDataIs13ThenIncrement [

	self skipOnPharoCITestingEnvironment.
	self assert: self class data equals: 13.
	self class incrementData
]

{ #category : 'initialization' }
DependencyTest >> testDataIs14ThenIncrement [

	self skipOnPharoCITestingEnvironment.
	self assert: self class data equals: 14.
	self class incrementData
]

{ #category : 'initialization' }
DependencyTest >> testDataIs15ThenIncrement [

	self skipOnPharoCITestingEnvironment.
	self assert: self class data equals: 15
]
