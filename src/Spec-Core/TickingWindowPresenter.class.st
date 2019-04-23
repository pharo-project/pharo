"
In addition to my superclass, I provide an API to execute the #step method every #stepTime.
"
Class {
	#name : #TickingWindowPresenter,
	#superclass : #WindowPresenter,
	#category : #'Spec-Core-Support'
}

{ #category : #specs }
TickingWindowPresenter class >> adapterName [

	^ #TickingWindowAdapter
]

{ #category : #stepping }
TickingWindowPresenter >> step [
	self presenter step
]

{ #category : #stepping }
TickingWindowPresenter >> stepTime [
	^ self presenter stepTime
]
