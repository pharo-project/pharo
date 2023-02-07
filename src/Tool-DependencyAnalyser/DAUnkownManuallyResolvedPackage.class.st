"
I am a Warning raised to the user if the list of manually resolved dependencies contains a package that does not exist in the system.
"
Class {
	#name : #DAUnkownManuallyResolvedPackage,
	#superclass : #Warning,
	#instVars : [
		'package'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #signalling }
DAUnkownManuallyResolvedPackage class >> signalOn: aPackageName [

	^ self new
		  package: aPackageName;
		  signal
]

{ #category : #accessing }
DAUnkownManuallyResolvedPackage >> messageText [
	^ messageText
		ifNil: [ package , ' dependency declared in the package Manifest as manuallyResolvedDependencies not detected in the system!' ]
]

{ #category : #accessing }
DAUnkownManuallyResolvedPackage >> package [

	^ package
]

{ #category : #accessing }
DAUnkownManuallyResolvedPackage >> package: anObject [

	package := anObject
]
