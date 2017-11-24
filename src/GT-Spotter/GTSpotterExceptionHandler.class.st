Class {
	#name : #GTSpotterExceptionHandler,
	#superclass : #Object,
	#category : #'GT-Spotter-Exceptions'
}

{ #category : #public }
GTSpotterExceptionHandler >> handleException: anException [
	^ self handleException: anException do: [ 
		('[Spotter] Exception: ', anException asString) logCr ]
]

{ #category : #public }
GTSpotterExceptionHandler >> handleException: anException do: aBlock [
	^ aBlock value
]

{ #category : #public }
GTSpotterExceptionHandler >> handleException: anException with: anObject [
	^ self handleException: anException do: [ anObject ]
]

{ #category : #handling }
GTSpotterExceptionHandler >> handleExceptionsDuring: aBlock [
	[ GTCurrentSpotterExceptionHandler use: self during: aBlock ] 
		on: self 
		do: [ :exception | 
			[ self handleException: exception ] " handle any exception during aBlock "
				on: Error
				do: [ :fatalException | self handleFatalException: fatalException ] ] " handle exception caused while handling an exception "
]

{ #category : #private }
GTSpotterExceptionHandler >> handleFatalException: anException [
	('[Spotter] Fatal Exception: ', anException asString) logCr
]
