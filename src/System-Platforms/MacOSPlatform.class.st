"
I am a an object representing a Mac OS (pre OSX) platform. Use myself to access platform specific features.

"
Class {
	#name : #MacOSPlatform,
	#superclass : #OSPlatform,
	#category : #'System-Platforms-Mac'
}

{ #category : #testing }
MacOSPlatform class >> isMacOS [

	^ Smalltalk vm operatingSystemName = 'Mac OS'
]

{ #category : #visiting }
MacOSPlatform >> accept: aVisitor [

	^ aVisitor visitMacOS: self
]

{ #category : #accessing }
MacOSPlatform >> defaultMaximumPathLength [
		"This method returns the default maximum path length for the getCurrentWorkingDirectory implementation. MacOS default PATH_MAX is 1024.  PATH_MAX is needed to avoid buffer overflow. In case PATH_MAX is not sufficient the method currentWorkingDirectoryWithBufferSize: can be used to give desired buffer size."
	 ^1024
]

{ #category : #accessing }
MacOSPlatform >> family [
	^#MacOS
]

{ #category : #testing }
MacOSPlatform >> isMacOS [
	^ true
]

{ #category : #accessing }
MacOSPlatform >> keyForValue: aKeyValue [
	^KeyboardKey valueForMacOSXPlatform: aKeyValue
]

{ #category : #accessing }
MacOSPlatform >> lineEnding [

	^ String lf
]

{ #category : #accessing }
MacOSPlatform >> menuShortcutString [
	^ 'cmd'
]
