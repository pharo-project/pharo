Class {
	#name : #RGIncompatibleBehaviorTypeError,
	#superclass : #Error,
	#category : #'Ring-Core-Kernel'
}

{ #category : #accessing }
RGIncompatibleBehaviorTypeError >> messageText [

	^ 'This message cannot be sent. Incompatible behavior type.'
]
