"
I am a toolbar used to navigate the results tree of the profiling
"
Class {
	#name : #ProfilerToolbarPresenter,
	#superclass : #SpToolbarPresenter,
	#instVars : [
		'selectedItem'
	],
	#category : #'ProfilerUI-View'
}

{ #category : #accessing }
ProfilerToolbarPresenter >> selectedItem [

	^ selectedItem
]

{ #category : #accessing }
ProfilerToolbarPresenter >> selectedItem: anObject [

	selectedItem := anObject
]
