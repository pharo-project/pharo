"
When there is a change in the class structure I announce that change.
I announce with a copy of the old class and the new class.
"
Class {
	#name : 'ShClassChanged',
	#superclass : 'ShAbstractChange',
	#category : 'Shift-ClassBuilder-Changes',
	#package : 'Shift-ClassBuilder',
	#tag : 'Changes'
}

{ #category : 'announcing' }
ShClassChanged >> announceChanges [

	self class codeChangeAnnouncer
		classDefinitionChangedFrom: oldClass to: builder newClass;
		classModificationAppliedTo: builder newClass
]

{ #category : 'announcing' }
ShClassChanged >> builder: anObject [
	builder := anObject.
	oldClass := builder oldClass copyForAnnouncement
]

{ #category : 'propagating' }
ShClassChanged >> propagateToSubclasses: anotherBuilder [

	anotherBuilder changes add: (ShSuperclassChanged new
		builder: builder;
		oldClass: oldClass;
		yourself
	)
]
