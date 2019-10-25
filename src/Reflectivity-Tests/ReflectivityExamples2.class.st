"
Examples for RF per object.
"
Class {
	#name : #ReflectivityExamples2,
	#superclass : #Object,
	#instVars : [
		'instVar'
	],
	#classVars : [
		'classVar'
	],
	#category : #'Reflectivity-Tests'
}

{ #category : #example }
ReflectivityExamples2 >> exampleMethod [
	^ 2 + 3
]

{ #category : #example }
ReflectivityExamples2 >> exampleMethod2 [
	^ (WriteStream on: String new)
		nextPutAll: 'hello world';
		contents
]

{ #category : #generation }
ReflectivityExamples2 >> generateNewMethodWithInstVarAccess [
	self class
		compile:
			'newMethodWithInstVarAccess
	instVar := 5.
	instVar := 6.
	instVar > 5 ifTrue:[^ instVar raisedTo: 2]'
		classified: 'example'
]

{ #category : #example }
ReflectivityExamples2 >> methodWithClassVarAccess [
	classVar := 5.
	classVar := 6.
	classVar > 5 ifTrue:[^ classVar raisedTo: 2]
	

]

{ #category : #example }
ReflectivityExamples2 >> methodWithInstVarAccess [
	instVar := 5.
	instVar := 6.
	instVar > 5 ifTrue:[^ instVar raisedTo: 2]
	

]

{ #category : #example }
ReflectivityExamples2 >> methodWithOverrides [
	^'top'
]

{ #category : #example }
ReflectivityExamples2 >> methodWithOverrides: str1 with: str2 [
	^str1, str2
]

{ #category : #example }
ReflectivityExamples2 >> methodWithTempVarAccess [
	|temp|
	temp := 5.
	temp := 6.
	temp > 5 ifTrue:[^ temp raisedTo: 2]
	

]

{ #category : #example }
ReflectivityExamples2 >> modifyMethodWithInstVarAccess [
	self class compile:'modifiedMethodWithInstVarAccess
	instVar > 5 ifTrue:[^ instVar raisedTo: 2]'
	

]

{ #category : #example }
ReflectivityExamples2 >> newMethodWithInstVarAccess [
	instVar := 5.
	instVar := 6.
	instVar > 5 ifTrue:[^ instVar raisedTo: 2]
]

{ #category : #removing }
ReflectivityExamples2 >> removeModifiedMethodWithInstVarAccess [
	self class removeSelector: #modifiedMethodWithInstVarAccess
]

{ #category : #removing }
ReflectivityExamples2 >> removeNewMethodWithInstVarAccess [
	self class removeSelector: #newMethodWithInstVarAccess
]

{ #category : #generation }
ReflectivityExamples2 >> resetModifiedMethodWithInstVarAccess [
	self class
		compile:
			'modifiedMethodWithInstVarAccess
	instVar := 5.
	instVar := 6.
	instVar > 5 ifTrue:[^ instVar raisedTo: 2]'
		classified: 'example'
]
