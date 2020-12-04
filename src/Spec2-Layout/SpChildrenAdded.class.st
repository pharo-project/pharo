Class {
	#name : #SpChildrenAdded,
	#superclass : #Announcement,
	#instVars : [
		'parent',
		'child'
	],
	#category : #'Spec2-Layout-Base'
}

{ #category : #accessing }
SpChildrenAdded >> child [
	^ child
]

{ #category : #accessing }
SpChildrenAdded >> child: aPresenter [ 
	child := aPresenter
]

{ #category : #accessing }
SpChildrenAdded >> parent: aPresenter [ 
	parent := aPresenter
]
