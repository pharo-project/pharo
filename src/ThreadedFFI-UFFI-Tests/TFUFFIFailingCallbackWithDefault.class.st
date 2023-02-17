"
I am a failing callback with default value for test usage only
"
Class {
	#name : #TFUFFIFailingCallbackWithDefault,
	#superclass : #TFUFFIAbstractCallback,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #initialization }
TFUFFIFailingCallbackWithDefault >> initialize [

	self signature: #(int (int)) block: [ :a | self error: 'An error in a callback' ]
]

{ #category : #initialization }
TFUFFIFailingCallbackWithDefault >> returnOnError [

	^ -1
]

{ #category : #initialization }
TFUFFIFailingCallbackWithDefault >> shouldDebugExceptions [

	^ false
]
