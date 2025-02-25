"
When there is a change in the metaclass structure I announce that change.
I announce with a copy of the old class and the new class.
"
Class {
	#name : 'ShMetaclassChanged',
	#superclass : 'ShAbstractChange',
	#category : 'Shift-ClassBuilder-Changes',
	#package : 'Shift-ClassBuilder',
	#tag : 'Changes'
}

{ #category : 'announcing' }
ShMetaclassChanged >> announceChanges [

	self class codeChangeAnnouncer
		classDefinitionChangedFrom: oldClass class to: builder newMetaclass;
		classModificationAppliedTo: builder newMetaclass
]

{ #category : 'accessing' }
ShMetaclassChanged >> builder: anObject [
	super builder: anObject.
	oldClass := builder oldClass copyForAnnouncement
]

{ #category : 'propagating' }
ShMetaclassChanged >> hasChangeClassSideShape [
	^ true
]

{ #category : 'propagating' }
ShMetaclassChanged >> propagateToSubclasses: anotherBuilder [
	anotherBuilder changes add: self
]
