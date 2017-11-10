Class {
	#name : #RBClassToRename,
	#superclass : #Model,
	#instVars : [
		'rewriteRule',
		'builder',
		'class'
	],
	#classInstVars : [
		'notUsed'
	],
	#category : #Refactoring-Tests-Core-Data
}

{ #category : #performing }
RBClassToRename >> method1 [
	^self method2
]

{ #category : #performing }
RBClassToRename >> method2 [
	^self method1
]
