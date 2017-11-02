"
I represent a change in the comment of a class or a trait.
"
Class {
	#name : #EpBehaviorCommentChange,
	#superclass : #EpBehaviorChange,
	#instVars : [
		'behavior',
		'oldComment',
		'newComment',
		'oldStamp',
		'newStamp'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpBehaviorCommentChange class >> newWith: aClassCommentedAnnouncement [ 

	^self
		newWithBehavior: aClassCommentedAnnouncement classCommented asEpiceaRingDefinition
		oldComment: aClassCommentedAnnouncement oldComment asString
		newComment: aClassCommentedAnnouncement newComment asString
		oldStamp: aClassCommentedAnnouncement oldStamp
		newStamp: aClassCommentedAnnouncement newStamp
]

{ #category : #'instance creation' }
EpBehaviorCommentChange class >> newWithBehavior: aBehavior oldComment: anOldComment newComment: aNewComment oldStamp: anOldStamp newStamp: aNewStamp [

	^ self basicNew
		initializeWithBehavior:aBehavior
		oldComment: anOldComment
		newComment: aNewComment
		oldStamp: anOldStamp
		newStamp: aNewStamp;
		yourself

]

{ #category : #visitor }
EpBehaviorCommentChange >> accept: aChangeVisitor [ 
	^ aChangeVisitor visitBehaviorCommentChange: self
]

{ #category : #accessing }
EpBehaviorCommentChange >> behaviorAffected [
	^ behavior
]

{ #category : #accessing }
EpBehaviorCommentChange >> behaviorAffectedName [
	^ behavior name
]

{ #category : #initialization }
EpBehaviorCommentChange >> initializeWithBehavior: aBehavior oldComment: anOldComment newComment: aNewComment oldStamp: anOldStamp newStamp: aNewStamp [

	self initialize.
	behavior := aBehavior.
	oldComment := anOldComment.
	newComment := aNewComment.
	oldStamp := anOldStamp.
	newStamp := aNewStamp.
]

{ #category : #accessing }
EpBehaviorCommentChange >> newComment [
	^ newComment
]

{ #category : #accessing }
EpBehaviorCommentChange >> newStamp [
	^ newStamp
]

{ #category : #accessing }
EpBehaviorCommentChange >> oldComment [
	^ oldComment
]

{ #category : #accessing }
EpBehaviorCommentChange >> oldStamp [
	^ oldStamp
]
