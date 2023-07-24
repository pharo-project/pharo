"
I am a warning that is raised in the case the user try to classify a method under an extension protocol that points the package containing the class. This should not happen because it would cause some incoherences in the system.
"
Class {
	#name : #ExtensionProtocolPointsOwningPackageWarning,
	#superclass : #Warning,
	#instVars : [
		'packageName'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #signalling }
ExtensionProtocolPointsOwningPackageWarning class >> signalFor: aPackageName [

	^ self new
		  packageName: aPackageName;
		  signal
]

{ #category : #accessing }
ExtensionProtocolPointsOwningPackageWarning >> messageText [

	^ messageText ifNil: [
		  'An extension protocol points the package "' , packageName
		  , '" while the class containing the method is already in this package. The creation of this protocol will be aborted.' ]
]

{ #category : #accessing }
ExtensionProtocolPointsOwningPackageWarning >> packageName [

	^ packageName
]

{ #category : #accessing }
ExtensionProtocolPointsOwningPackageWarning >> packageName: anObject [

	packageName := anObject
]
