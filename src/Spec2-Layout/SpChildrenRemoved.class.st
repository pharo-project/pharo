Class {
	#name : #SpChildrenRemoved,
	#superclass : #Announcement,
	#instVars : [
		'parent',
		'child'
	],
	#category : #'Spec2-Layout-Base'
}

{ #category : #accessing }
SpChildrenRemoved >> child [
	^ child
]

{ #category : #accessing }
SpChildrenRemoved >> child: aPresenter [ 
	child := aPresenter
]

{ #category : #accessing }
SpChildrenRemoved >> parent: aPresenter [ 
	parent := aPresenter
]
