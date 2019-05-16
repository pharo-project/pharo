Class {
	#name : #SpecApplicationConfiguration,
	#superclass : #Object,
	#category : #'Spec-Core-Base'
}

{ #category : #configuring }
SpecApplicationConfiguration >> configure: anApplication [
	"Override this to add your application configuration"
	
	OSPlatform current 
		configureApplication: anApplication 
		configuration: self
]

{ #category : #configuring }
SpecApplicationConfiguration >> configureOSX: anApplication [
]

{ #category : #configuring }
SpecApplicationConfiguration >> configureUnix: anApplication [
]

{ #category : #configuring }
SpecApplicationConfiguration >> configureWindows: anApplication [
]
