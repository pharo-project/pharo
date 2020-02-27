"
A variable definition
"
Class {
	#name : #RGVariableDefinition,
	#superclass : #RGElementDefinition,
	#category : #'Ring-Definitions-Core-Base'
}

{ #category : #comparing }
RGVariableDefinition >> = aRGVariableDefinition [
	"This variable look for equality of the key properties of the receiver"

	^(super = aRGVariableDefinition)
		and:[ self name == aRGVariableDefinition name ]
]

{ #category : #accessing }
RGVariableDefinition >> environment [

	^ self parent environment 
]

{ #category : #accessing }
RGVariableDefinition >> fullName [
	"Keeps a unique description for the receiver. As annotation to avoid converting each time is invoked"
	
	^self annotationNamed: self class fullNameKey
		ifAbsentPut:[ (self parentName, '->', self name) asSymbol ]
]

{ #category : #comparing }
RGVariableDefinition >> hash [

	^super hash bitXor: self name hash
]

{ #category : #testing }
RGVariableDefinition >> isClassInstanceVariable [

	^false
]

{ #category : #testing }
RGVariableDefinition >> isClassVariable [

	^false
]

{ #category : #testing }
RGVariableDefinition >> isInstanceVariable [

	^false
]

{ #category : #testing }
RGVariableDefinition >> isPoolVariable [

	^false
]

{ #category : #testing }
RGVariableDefinition >> isVariable [

	^true
]
