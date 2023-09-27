Class {
	#name : 'RBClassToRename',
	#superclass : 'Model',
	#category : 'Refactoring-DataForTesting-MiniHierarchy',
	#package : 'Refactoring-DataForTesting',
	#tag : 'MiniHierarchy'
}

{ #category : 'testing' }
RBClassToRename class >> justForTest [
	^ 42
]

{ #category : 'performing' }
RBClassToRename >> equallyDefinedInSubclass [

	^ self method1 + 33
]

{ #category : 'performing' }
RBClassToRename >> justSuperSendInSubclass [

	^ self method1 + 33
]

{ #category : 'performing' }
RBClassToRename >> method1 [
	^self method2
]

{ #category : 'performing' }
RBClassToRename >> method2 [
	^self method1
]
