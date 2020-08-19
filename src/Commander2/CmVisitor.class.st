"
I am the implementation of the visitor design pattern for commands and commands group.
I am abstract, my subclasses should at least implement `#visitCommand:`.
I define default behaviour for visiting a command group.
"
Class {
	#name : #CmVisitor,
	#superclass : #Object,
	#category : #'Commander2-Visitors'
}

{ #category : #visiting }
CmVisitor >> visit: anObject [
	^ anObject acceptVisitor: self
]

{ #category : #visiting }
CmVisitor >> visitCommand: aCmCommandEntry [
	^ self subclassResponsibility
]

{ #category : #visiting }
CmVisitor >> visitCommandGroup: aCmCommandsGroup [
	^ aCmCommandsGroup entries collect: [ :entry |
		entry acceptVisitor: self ]
]
