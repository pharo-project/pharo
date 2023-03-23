"
I am a an object representing a MacOSX platform. Use myself to access platform specific features.

Please keep me polymorphic to the the other platform objects.

To get the current platform, you can evaluate:

OSPlatform current.
"
Class {
	#name : #MacOSXPlatform,
	#superclass : #MacOSPlatform,
	#category : #'System-Platforms-Mac'
}

{ #category : #testing }
MacOSXPlatform class >> isActivePlatform [
	^ self isMacOS
]

{ #category : #visiting }
MacOSXPlatform >> accept: aVisitor [

	^ aVisitor visitMacOSX: self
]

{ #category : #accessing }
MacOSXPlatform >> family [
	^#MacOSX
]

{ #category : #'error handling' }
MacOSXPlatform >> getErrorMessage: errnum [

	^ self ffiCall: #(char *strerror(int errnum)) module: #LibC
]

{ #category : #testing }
MacOSXPlatform >> isMacOSX [
	^ true
]

{ #category : #testing }
MacOSXPlatform >> isTranslated [
	"Tells if the Pharo VM is translated by Rosetta.
	See https://developer.apple.com/documentation/apple-silicon/about-the-rosetta-translation-environment#3616845 for more details.
	If it's translated:
	$ sysctl sysctl.proc_translated
	sysctl.proc_translated: 1

	If not translated:
	$ sysctl sysctl.proc_translated
	sysctl.proc_translated: 0

	On non-ARM Macs:
	$ sysctl sysctl.proc_translated
	sysctl: unknown oid 'sysctl.proc_translated'
	"

	^ (LibC resultOfCommand: 'sysctl -n sysctl.proc_translated') trim = '1'
]

{ #category : #accessing }
MacOSXPlatform >> processorArchitecture [

	^ self isTranslated
		ifTrue: [ 'arm64' ]
		ifFalse: [ super processorArchitecture ]
]
