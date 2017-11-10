"
A MCClassVariableDefinition represent a class variable.
"
Class {
	#name : #MCClassVariableDefinition,
	#superclass : #MCVariableDefinition,
	#category : #Monticello-Modeling
}

{ #category : #accessing }
MCClassVariableDefinition class >> type [
	^ #class
]

{ #category : #testing }
MCClassVariableDefinition >> isClassVariable [
	^ true
]

{ #category : #testing }
MCClassVariableDefinition >> isOrderDependend [
	^false
]
