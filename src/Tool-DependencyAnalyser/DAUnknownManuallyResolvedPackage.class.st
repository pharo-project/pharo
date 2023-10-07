"
I am a Warning raised to the user if the list of manually resolved dependencies contains a package that does not exist in the system.
"
Class {
	#name : 'DAUnknownManuallyResolvedPackage',
	#superclass : 'Warning',
	#instVars : [
		'package'
	],
	#category : 'Tool-DependencyAnalyser-Core',
	#package : 'Tool-DependencyAnalyser',
	#tag : 'Core'
}

{ #category : 'signalling' }
DAUnknownManuallyResolvedPackage class >> signalOn: aPackageName [

	^ self new
		  package: aPackageName;
		  signal
]

{ #category : 'accessing' }
DAUnknownManuallyResolvedPackage >> messageText [
	^ messageText
		ifNil: [ package , ' dependency declared in the package Manifest as manuallyResolvedDependencies not detected in the system!' ]
]

{ #category : 'accessing' }
DAUnknownManuallyResolvedPackage >> package [

	^ package
]

{ #category : 'accessing' }
DAUnknownManuallyResolvedPackage >> package: anObject [

	package := anObject
]
