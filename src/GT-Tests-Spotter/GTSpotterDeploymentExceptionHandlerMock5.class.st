Class {
	#name : #GTSpotterDeploymentExceptionHandlerMock5,
	#superclass : #GTSpotterDeploymentExceptionHandlerMock,
	#category : #'GT-Tests-Spotter-Exceptions-Mocks'
}

{ #category : #public }
GTSpotterDeploymentExceptionHandlerMock5 >> handleException: anException do: aBlock [
	super handleException: anException do: aBlock. " we perform the block because mock5 should not raise any exception within aBlock "
	^ aBlock value
]
