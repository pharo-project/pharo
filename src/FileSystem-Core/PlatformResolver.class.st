"
I am an abstract superclass for platform-specific resolvers.
"
Class {
	#name : #PlatformResolver,
	#superclass : #FileSystemResolver,
	#category : #'FileSystem-Core-Base-Resolver'
}

{ #category : #'instance creation' }
PlatformResolver class >> forCurrentPlatform [
	| platformName |
	platformName :=  Smalltalk os platformName.
	^ (self allSubclasses detect: [:ea | ea platformName = platformName]) new
]

{ #category : #accessing }
PlatformResolver class >> platformName [
	^ nil
]

{ #category : #private }
PlatformResolver class >> primitiveGetUntrustedUserDirectory [
	<primitive: 'primitiveGetUntrustedUserDirectory' module: 'SecurityPlugin'>
	self primitiveFailed.
	self flag: #pharoFixMe. "use a more decent way to get a path in the users home directory"
]

{ #category : #origins }
PlatformResolver >> cache [
	"Operating Systems often define standard locations for a personal cache directory. The cache directory is a user-specific non-essential (cached) place where data should be written."

	<origin>
	^ self subclassResponsibility
]

{ #category : #private }
PlatformResolver >> cantFindOriginError [
	^ Error signal: 'Can''t find the requested origin' 
]

{ #category : #origins }
PlatformResolver >> desktop [
	^ self home / 'Desktop'
]

{ #category : #private }
PlatformResolver >> directoryFromEnvVariableNamed: aString [
	^ self directoryFromEnvVariableNamed: aString or: [ self cantFindOriginError ]
]

{ #category : #private }
PlatformResolver >> directoryFromEnvVariableNamed: aString or: aBlock [
	| envValue |
	envValue := [ Smalltalk os environment at: aString ifAbsent: [ nil ] ]
		on: Error
		do: [ ^ aBlock value ].
	^ envValue isEmptyOrNil
		ifTrue: [ aBlock value ]
		ifFalse: [ self resolveString: envValue ]
]

{ #category : #origins }
PlatformResolver >> documents [
	<origin>
	^ self home / 'Documents'
]

{ #category : #origins }
PlatformResolver >> downloads [
	<origin>
	^ self home / 'Downloads'
]

{ #category : #origins }
PlatformResolver >> home [
	<origin>
	^ self subclassResponsibility
]

{ #category : #origins }
PlatformResolver >> preferences [
	<origin>
	^ self subclassResponsibility
]

{ #category : #origins }
PlatformResolver >> temp [
	"Where to put files that are not supposed to last long"

	<origin>
	^ self subclassResponsibility
]
