"
I represent an item in the navigation toolbar used to display the profiling results
"
Class {
	#name : #ProfilerToolbarButtonPresenter,
	#superclass : #SpToolbarButtonPresenter,
	#instVars : [
		'object',
		'selectedPath'
	],
	#category : #'ProfilerUI-View'
}

{ #category : #accessing }
ProfilerToolbarButtonPresenter >> object [

	^ object
]

{ #category : #accessing }
ProfilerToolbarButtonPresenter >> object: anObject [

	object := anObject
]

{ #category : #printing }
ProfilerToolbarButtonPresenter >> printOn: aStream [

	aStream nextPutAll: self object methodName
]

{ #category : #accessing }
ProfilerToolbarButtonPresenter >> selectedPath [
	^ selectedPath
]

{ #category : #accessing }
ProfilerToolbarButtonPresenter >> selectedPath: aCollection [ 

	selectedPath := aCollection
]
