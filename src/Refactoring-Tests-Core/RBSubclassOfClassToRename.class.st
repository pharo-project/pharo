Class {
	#name : #RBSubclassOfClassToRename,
	#superclass : #RBClassToRename,
	#instVars : [
		'rewriteRule1'
	],
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #accessing }
RBSubclassOfClassToRename >> calls [
	^self rewriteRule1: self name , self rewriteRule1
]

{ #category : #accessing }
RBSubclassOfClassToRename >> calls1 [
	^self rewriteRule1: (self rewriteRule1: self calls)
]

{ #category : #accessing }
RBSubclassOfClassToRename >> name [
	^rewriteRule1
]

{ #category : #performing }
RBSubclassOfClassToRename >> reference [
	^ RBClassToRename new
]

{ #category : #accessing }
RBSubclassOfClassToRename >> rewriteRule1 [
	^rewriteRule1
]

{ #category : #accessing }
RBSubclassOfClassToRename >> rewriteRule1: anObject [
	^rewriteRule1 := anObject
]

{ #category : #performing }
RBSubclassOfClassToRename >> symbolReference [
	^ #RBClassToRename
]
