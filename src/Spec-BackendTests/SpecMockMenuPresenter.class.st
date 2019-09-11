Class {
	#name : #SpecMockMenuPresenter,
	#superclass : #MenuPresenter,
	#instVars : [
		'shown'
	],
	#category : #'Spec-BackendTests'
}

{ #category : #'as yet unclassified' }
SpecMockMenuPresenter >> buildWithSpec [

	^ SpecMockMenu new
		realMenu: super buildWithSpec;
		whenShownDo: [ shown := true ];
		yourself
]

{ #category : #initialization }
SpecMockMenuPresenter >> initialize [ 

	super initialize.
	shown := false.
]

{ #category : #accessing }
SpecMockMenuPresenter >> shown [
	^ shown
]

{ #category : #accessing }
SpecMockMenuPresenter >> shown: anObject [
	^ shown := anObject
]
