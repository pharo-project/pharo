"
I represent an item in the navigation toolbar used to display the profiling results
"
Class {
	#name : 'ProfilerToolbarButtonPresenter',
	#superclass : 'SpToolbarButtonPresenter',
	#instVars : [
		'object',
		'selectedObject'
	],
	#category : 'ProfilerUI-View',
	#package : 'ProfilerUI',
	#tag : 'View'
}

{ #category : 'accessing' }
ProfilerToolbarButtonPresenter >> object [

	^ object
]

{ #category : 'accessing' }
ProfilerToolbarButtonPresenter >> object: anObject [

	object := anObject
]

{ #category : 'printing' }
ProfilerToolbarButtonPresenter >> printOn: aStream [

	aStream nextPutAll: self object methodName
]

{ #category : 'accessing' }
ProfilerToolbarButtonPresenter >> selectedObject [

	^ selectedObject
]

{ #category : 'accessing' }
ProfilerToolbarButtonPresenter >> selectedObject: aTallyModel [

	selectedObject := aTallyModel
]
