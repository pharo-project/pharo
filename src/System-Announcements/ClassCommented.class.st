"
This announcement will be emitted when a class or a trait comment changes
"
Class {
	#name : #ClassCommented,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'newStamp',
		'newComment',
		'classCommented',
		'oldComment',
		'oldStamp'
	],
	#category : #'System-Announcements-System-Classes'
}

{ #category : #'instance creation' }
ClassCommented class >> class: aClass oldComment: oldComment newComment: newComment oldStamp: oldStamp newStamp: newStamp [
	^self new
			classCommented: aClass;
			oldComment: oldComment;
			oldStamp: oldStamp;
			newComment: newComment;
			newStamp: newStamp;
			yourself
]

{ #category : #'instance creation' }
ClassCommented class >> classCommented: aClass [
	self flag: #pharoFixMe. "nils? no way! "
	^self class: aClass oldComment: nil newComment: nil oldStamp: nil newStamp: nil
]

{ #category : #accessing }
ClassCommented >> classAffected [
	^self classCommented
]

{ #category : #accessing }
ClassCommented >> classCommented [
	^classCommented
]

{ #category : #accessing }
ClassCommented >> classCommented: aClass [
	classCommented := aClass
]

{ #category : #accessing }
ClassCommented >> newComment [
	^newComment
]

{ #category : #accessing }
ClassCommented >> newComment: aNewComment [
	newComment := aNewComment
]

{ #category : #accessing }
ClassCommented >> newStamp [
	^newStamp
]

{ #category : #accessing }
ClassCommented >> newStamp: aNewStamp [
	newStamp := aNewStamp
]

{ #category : #accessing }
ClassCommented >> oldComment [
	^ oldComment
]

{ #category : #accessing }
ClassCommented >> oldComment: andOldComment [
	oldComment := andOldComment
]

{ #category : #accessing }
ClassCommented >> oldStamp [
	^ oldStamp
]

{ #category : #accessing }
ClassCommented >> oldStamp: anOldStamp [
	oldStamp := anOldStamp
]
