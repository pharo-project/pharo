"
I am a model representing a transfer during a drag and drop operation. I keep a pointer the source of the drag, and a pointer to what is dragged
"
Class {
	#name : #TransferPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'passenger',
		'source'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #specs }
TransferPresenter class >> adapterName [

	^ #TransferAdapter
]

{ #category : #accessing }
TransferPresenter >> from: aModel [

	source value: aModel
]

{ #category : #initialize }
TransferPresenter >> initialize [

	super initialize.

	passenger := nil asValueHolder.
	source := nil asValueHolder.
]

{ #category : #accessing }
TransferPresenter >> passenger [

	^ passenger value
]

{ #category : #accessing }
TransferPresenter >> source [

	^ source value
]

{ #category : #accessing }
TransferPresenter >> transfer: anObject [

	passenger value: anObject
]
