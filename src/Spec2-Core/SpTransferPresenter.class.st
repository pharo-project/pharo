"
I am a model representing a transfer during a drag and drop operation. I keep a pointer the source of the drag, and a pointer to what is dragged
"
Class {
	#name : #SpTransferPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'passenger',
		'source'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #specs }
SpTransferPresenter class >> adapterName [

	^ #TransferAdapter
]

{ #category : #accessing }
SpTransferPresenter >> from: aModel [

	source value: aModel
]

{ #category : #initialization }
SpTransferPresenter >> initialize [

	super initialize.

	passenger := nil asValueHolder.
	source := nil asValueHolder.
]

{ #category : #accessing }
SpTransferPresenter >> passenger [

	^ passenger value
]

{ #category : #accessing }
SpTransferPresenter >> source [

	^ source value
]

{ #category : #accessing }
SpTransferPresenter >> transfer: anObject [

	passenger value: anObject
]
