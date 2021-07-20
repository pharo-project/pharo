"
An OSPlatform is an abstract representation of a 'OS platform'.
Platforms can be hierarchical, e.g., a ""general"" platform as superclass and more specific platforms as subclasses as long as the subclasses provide sufficient means to identify themselves.
The original implementation was for Tweak.

Current		holds the current OSPlatform subclass

Architectural considerations:
most platform specific methods that need to be added to the platform class should be in the form of extensions rather then adding them directly to this package. Otherwise the platform class will degenerate very quickly into a dependence hub for all kinds of sub systems.
"
Class {
	#name : #OSPlatform,
	#superclass : #Object,
	#classVars : [
		'Current'
	],
	#category : #'System-Platforms-Base'
}

{ #category : #accessing }
OSPlatform class >> current [
	"Answer the current platform"
	^Current
]

{ #category : #'system attributes' }
OSPlatform class >> currentPlatformName [
	"Return the name of the platform we're running on"

	^ Smalltalk vm getSystemAttribute: 1001
]

{ #category : #'system attributes' }
OSPlatform class >> currentVersion [
	"Return the version number string of the platform we're running on"

	^ (Smalltalk vm getSystemAttribute: 1002) asString
]

{ #category : #private }
OSPlatform class >> determineActivePlatform [
	"Look for the matching platform class"
	^ self allSubclasses 
		detect: [ :any | any isActivePlatform ].

]

{ #category : #examples }
OSPlatform class >> example [
	<example>
	Smalltalk os current inspect
]

{ #category : #'class initialization' }
OSPlatform class >> initialize [
	"OSPlatform initialize"

	SessionManager default
		registerSystemClassNamed: self name
		atPriority: 50.	
	self startUp: true.
]

{ #category : #private }
OSPlatform class >> isActivePlatform [
	"Answer whether the receiver is the active platform"
	^false
]

{ #category : #'system startup' }
OSPlatform class >> shutDown: isImageQuitting [
	"The system is going down"
	Current ifNotNil: [ Current shutDown: isImageQuitting ].

]

{ #category : #'system startup' }
OSPlatform class >> startUp: isImageStarting [
	"Determine the current platform."
	
	isImageStarting ifFalse: [  ^ self ].
	
	Current := self determineActivePlatform new.
	Current startUp: isImageStarting
]

{ #category : #visiting }
OSPlatform >> accept: aVisitor [

	self subclassResponsibility	
]

{ #category : #accessing }
OSPlatform >> current [
	^ self
]

{ #category : #accessing }
OSPlatform >> currentWorkingDirectoryPath [
	"We MUST decode raw bytes to support non-Ascii characters"
	^ self currentWorkingDirectoryRawPath decodeWith: self defaultEncoding
]

{ #category : #accessing }
OSPlatform >> currentWorkingDirectoryPathWithBuffer: aByteArray [
	<primitive: 'primitiveGetCurrentWorkingDirectory' module: '' error: ec>
	
	^ (self getPwdViaFFI: aByteArray size: aByteArray size) 
		ifNil:[ self error: 'Insufficient buffer size to read current working directory: ' , aByteArray size asString]
]

{ #category : #accessing }
OSPlatform >> currentWorkingDirectoryPathWithBufferSize: aBufferSize [
	| buffer |
	buffer := ByteArray new: aBufferSize.
	^ self currentWorkingDirectoryPathWithBuffer: buffer
]

{ #category : #accessing }
OSPlatform >> currentWorkingDirectoryRawPath [
	"This method calls the method getPwdViaFFI with argument of a buffer size. 
	By default it uses the defaultMaximumPathLength of each subclass as the buffer size.
	We return a raw byte array that should be decoded with the proper encoding."
	^ (self currentWorkingDirectoryPathWithBufferSize: self defaultMaximumPathLength) asByteArray

]

{ #category : #accessing }
OSPlatform >> defaultEncoding [

	^ ZnCharacterEncoder utf8
]

{ #category : #accessing }
OSPlatform >> defaultMaximumPathLength [
	"This method returns the default maximum path length for the getCurrentWorkingDirectory implementation"
	 ^self subclassResponsibility 

]

{ #category : #accessing }
OSPlatform >> family [
	"Returns a symbol specific to the platform family (MacOSX, Windows, Unix, RiscOS).
	This may need to be extended if there are new platforms added or significant differences within a platform family arise (as was the case between MacOS 9 and X)."

	self subclassResponsibility
]

{ #category : #accessing }
OSPlatform >> getPwdViaFFI: arg1 size: arg2 [
	" This is an virtual method. It is implemented in its sub classes"
	 ^ self subclassResponsibility 

]

{ #category : #testing }
OSPlatform >> isLinux [
	^ false
]

{ #category : #testing }
OSPlatform >> isMacOS [
	^ false
]

{ #category : #testing }
OSPlatform >> isMacOSX [
	^ false
]

{ #category : #testing }
OSPlatform >> isUnix [
	^ false
]

{ #category : #testing }
OSPlatform >> isUnix32 [
	^ false
]

{ #category : #testing }
OSPlatform >> isUnix64 [
	^ false
]

{ #category : #testing }
OSPlatform >> isWin32 [
	^ false
]

{ #category : #testing }
OSPlatform >> isWin64 [
	^ false
]

{ #category : #testing }
OSPlatform >> isWindows [
	^ false
]

{ #category : #testing }
OSPlatform >> isX11 [
	^ self windowSystemName = 'X11'
]

{ #category : #accessing }
OSPlatform >> lineEnding [

	^ self subclassResponsibility
]

{ #category : #accessing }
OSPlatform >> menuShortcutString [
	^ self subclassResponsibility
]

{ #category : #accessing }
OSPlatform >> name [
	"Return the name of the platform we're running on"

	^ self class currentPlatformName
]

{ #category : #compatbility }
OSPlatform >> platformFamily [
	^ self family
]

{ #category : #compatbility }
OSPlatform >> platformName [
	^ self name
]

{ #category : #initialize }
OSPlatform >> shutDown: quitting [
	"Pharo is shutting down. If this platform requires specific shutdown code, this is a great place to put it."

]

{ #category : #initialize }
OSPlatform >> startUp: resuming [
	"Pharo is starting up. If this platform requires specific initialization, this is a great place to put it."

]

{ #category : #accessing }
OSPlatform >> subtype [
	"Return the subType of the platform we're running on"

	^ (Smalltalk vm getSystemAttribute: 1003)
]

{ #category : #accessing }
OSPlatform >> version [
	"Return the version number string of the platform we're running on"

	^ self class currentVersion
]

{ #category : #accessing }
OSPlatform >> virtualKey: virtualKeyCode [
	"Subclass responsibility to override if necessary"
	^nil
]

{ #category : #accessing }
OSPlatform >> windowSystemName [			
	"Return the name of the window system currently being used for display."

	^ Smalltalk vm getSystemAttribute: 1005
]
