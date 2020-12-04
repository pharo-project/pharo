"
! Application Configurations

Tipically, each Spec application will implement one or several configurations (for example, to run on Morphic or Gtk) by extending this class or one of its children. 
A configuration takes the responsibility to prepare an application to run properly. This preparation can be different depending on the platform where it is running, that's why you have several extension points you can extend/override: 

- ==configure:== a generic configuration point that normally will dispatch the configuration to a plarform specific method. 
- ==configureOSX:==/==configureUnix:==/==configureWindows:== platform specific entry points.

@@TODO: Examples of configurations.

see also: *class:SpMorphicConfiguration*, *class:SpGtkConfiguration*
"
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
