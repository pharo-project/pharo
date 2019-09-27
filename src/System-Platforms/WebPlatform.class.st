Class {
	#name : #WebPlatform,
	#superclass : #OSPlatform,
	#category : #'System-Platforms-Web'
}

{ #category : #private }
WebPlatform class >> isActivePlatform [
	"Answer whether the receiver is the active platform"
	^ self currentPlatformName = 'Web'
]

{ #category : #accessing }
WebPlatform >> currentWorkingDirectoryPathWithBufferSize: ignored [
	^'/SqueakJS'
]

{ #category : #accessing }
WebPlatform >> defaultMaximumPathLength [
	"This method returns the default maximum path length for the getCurrentWorkingDirectory implementation"
	 ^4096
]

{ #category : #accessing }
WebPlatform >> family [
	"Returns a symbol specific to the platform family (MacOSX, Windows, Unix, RiscOS).
	This may need to be extended if there are new platforms added or significant differences within a platform family arise (as was the case between MacOS 9 and X)."

	#Web
]

{ #category : #accessing }
WebPlatform >> getPwdViaFFI: buffer size: bufferSize [
	buffer
		replaceFrom: 1
		to: '/SqueakJS' size
		with: '/SqueakJS';
		at: '/SqueakJS' size + 1
		put: (Character value: 0)
]

{ #category : #accessing }
WebPlatform >> lineEnding [

	^ String lf
]

{ #category : #accessing }
WebPlatform >> menuShortcutString [
	^ 'cmd'
]
