Class {
	#name : #SpAbstractBackendForTest,
	#superclass : #Object,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractBackendForTest >> emulateClick: mouseButtonCode onTest: anAdapterTest [ 

	anAdapterTest openInstance.

	MouseButtonEvent yellowButton = mouseButtonCode 
		ifTrue: [ anAdapterTest adapter rightClicked ].

	MouseButtonEvent redButton = mouseButtonCode 
		ifTrue: [ anAdapterTest adapter clicked ].

]
