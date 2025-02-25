Class {
	#name : 'RBSubclassOfClassToRename',
	#superclass : 'RBClassToRename',
	#instVars : [
		'rewriteRule1'
	],
	#category : 'Refactoring-DataForTesting-MiniHierarchy',
	#package : 'Refactoring-DataForTesting',
	#tag : 'MiniHierarchy'
}

{ #category : 'accessing' }
RBSubclassOfClassToRename >> calls [
	^self rewriteRule1: self name , self rewriteRule1
]

{ #category : 'accessing' }
RBSubclassOfClassToRename >> calls1 [
	^self rewriteRule1: (self rewriteRule1: self calls)
]

{ #category : 'performing' }
RBSubclassOfClassToRename >> equallyDefinedInSubclass [

	"Duplicate here on purpose for testing purposes"
	^ self method1 + 33
]

{ #category : 'accessing' }
RBSubclassOfClassToRename >> justSuperSendInSubclass [

	^ super justSuperSendInSubclass
]

{ #category : 'accessing' }
RBSubclassOfClassToRename >> justSuperSendInSubclassBroken [

	^ super justSuperSendInSubclass + 2
]

{ #category : 'accessing' }
RBSubclassOfClassToRename >> methodThatIsNotUsedForSure [

	
]

{ #category : 'accessing' }
RBSubclassOfClassToRename >> name [
	^rewriteRule1
]

{ #category : 'accessing' }
RBSubclassOfClassToRename >> printOn: aStream [

	"just for the test"
	super printOn: aStream.
	aStream nextPutAll: 'tintin'
]

{ #category : 'performing' }
RBSubclassOfClassToRename >> reference [
	^ RBClassToRename new
]

{ #category : 'accessing' }
RBSubclassOfClassToRename >> rewriteRule1 [
	^rewriteRule1
]

{ #category : 'accessing' }
RBSubclassOfClassToRename >> rewriteRule1: anObject [
	^rewriteRule1 := anObject
]

{ #category : 'performing' }
RBSubclassOfClassToRename >> symbolReference [
	^ #RBClassToRename
]
