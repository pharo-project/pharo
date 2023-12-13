"
I represent an item in the navigation toolbar used to display the profiling results
"
Class {
	#name : 'StProfilerToolbarButtonPresenter',
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
StProfilerToolbarButtonPresenter >> object [

	^ object
]

{ #category : 'accessing' }
StProfilerToolbarButtonPresenter >> object: anObject [

	object := anObject
]

{ #category : 'printing' }
StProfilerToolbarButtonPresenter >> printOn: aStream [

	aStream nextPutAll: self object methodName
]

{ #category : 'accessing' }
StProfilerToolbarButtonPresenter >> selectedObject [

	^ selectedObject
]

{ #category : 'accessing' }
StProfilerToolbarButtonPresenter >> selectedObject: aTallyModel [

	selectedObject := aTallyModel
]
