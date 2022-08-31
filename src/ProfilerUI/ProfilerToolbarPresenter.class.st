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
ProfilerToolbarPresenter >> getItemWith: anObject [

	^ (self items select: [ :item | item object == anObject ])
		  ifEmpty: [ nil ]
		  ifNotEmpty: [ :result | result anyOne ]
]

{ #category : #accessing }
ProfilerToolbarPresenter >> selectedItem [

	^ selectedItem
]

{ #category : #accessing }
ProfilerToolbarPresenter >> selectedItem: anObject [

	selectedItem := anObject
]

{ #category : #accessing }
ProfilerToolbarPresenter >> setSelectedItem: aToolbarButtonPresenter [ 
	
	self items do: [:itm | itm icon: nil ].
	aToolbarButtonPresenter icon: (self iconNamed: 'windowMenu').
	self selectedItem: aToolbarButtonPresenter
	
]
