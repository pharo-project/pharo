Class {
	#name : #SpApplicationConfiguration,
	#superclass : #Object,
	#category : #'Spec2-Core-Base'
}

{ #category : #configuring }
SpApplicationConfiguration >> configure: anApplication [
	"Override this to add your application configuration"
	
	OSPlatform current 
		configureApplication: anApplication 
		configuration: self
]

{ #category : #configuring }
SpApplicationConfiguration >> configureOSX: anApplication [
]

{ #category : #configuring }
SpApplicationConfiguration >> configureUnix: anApplication [
]

{ #category : #configuring }
SpApplicationConfiguration >> configureWindows: anApplication [
]
