Class {
	#name : #GTSpotterExceptionMock5Filter,
	#superclass : #GTFilter,
	#category : #'GT-Tests-Spotter-Exceptions-Mocks'
}

{ #category : #public }
GTSpotterExceptionMock5Filter >> value: aSpotterContext [
	" No Exceptions expected ! "
	| currentExceptionHandler |
	currentExceptionHandler := GTCurrentSpotterExceptionHandler value.
	self assert: currentExceptionHandler notNil.
	self assert: currentExceptionHandler == aSpotterContext exceptionHandler.
	self assert: currentExceptionHandler == aSpotterContext step exceptionHandler.
	self assert: currentExceptionHandler == aSpotterContext spotter exceptionHandler.
	^ #( 'done' )
]
