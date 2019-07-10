Class {
	#name : #AbstractBackendForTest,
	#superclass : #Object,
	#category : #'Spec-BackendTests'
}

{ #category : #testing }
AbstractBackendForTest >> emulateClick: mouseButtonCode onTest: anAdapterTest [ 

	anAdapterTest openInstance.

	MouseButtonEvent yellowButton = mouseButtonCode 
		ifTrue: [ anAdapterTest adapter rightClicked ].

	MouseButtonEvent redButton = mouseButtonCode 
		ifTrue: [ anAdapterTest adapter clicked ].

]
