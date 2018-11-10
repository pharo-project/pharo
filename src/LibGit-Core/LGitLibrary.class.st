Class {
	#name : #LGitLibrary,
	#superclass : #FFILibrary,
	#instVars : [
		'initialized'
	],
	#category : #'LibGit-Core-FFI-Handling'
}

{ #category : #'class initialization' }
LGitLibrary class >> initialize [
	"Required to reset the Initialized flag on image startup.
	Force libgit initialization since we just installed libgit.
	Otherwise this would cause a libgit crash"
	
	Smalltalk addToStartUpList: self.
	self uniqueInstance initializeLibGit2
]

{ #category : #'system startup' }
LGitLibrary class >> shutDown: quitting [

	quitting ifFalse: [ ^ self ].
	self uniqueInstance shutdownLibGit2.
]

{ #category : #'system startup' }
LGitLibrary class >> startUp: isImageStarting [

	isImageStarting ifTrue: [ self uniqueInstance initializeLibGit2 ]
]

{ #category : #operations }
LGitLibrary >> forgetInitializationState [

	initialized := nil
]

{ #category : #operations }
LGitLibrary >> initializeLibGit2 [
	[ self libgit2_init.
	self recordInitializationSuccess ]
		on: Error
		do: [ :ex | 
			self recordInitializationFailure.
			ex pass ]
]

{ #category : #testing }
LGitLibrary >> isInitialized [

	^initialized == true
]

{ #category : #'libgit-calls' }
LGitLibrary >> libgit2_features [
	"Query compile time options for libgit2."
	
	^self ffiCall: #(int git_libgit2_features())
]

{ #category : #'libgit-calls' }
LGitLibrary >> libgit2_featuresAlt [
	"Query compile time options for libgit2."
	
	| fnc |
	
	fnc := ExternalLibraryFunction
		name: 'git_libgit2_features'
		module: 'libgit2.0.dylib'
		callType: 0
		returnType: FFIInt32 new externalTypeWithArity
		argumentTypes: {}.
		
	^fnc invokeWithArguments: { }

]

{ #category : #'libgit-calls' }
LGitLibrary >> libgit2_init [
	^self ffiCall: #(int git_libgit2_init())
]

{ #category : #'libgit-calls' }
LGitLibrary >> libgit2_shutdown [
	^self ffiCall: #(int git_libgit2_shutdown())
]

{ #category : #'libgit-calls' }
LGitLibrary >> libgit2_version: major minor: minor rev: rev [
	"Return the version of the libgit2 library being currently used."
	^self ffiCall: #(void git_libgit2_version(LGitWriteBackValueInt major, LGitWriteBackValueInt minor, LGitWriteBackValueInt rev))
]

{ #category : #'accessing platform' }
LGitLibrary >> macModuleName [
	| pluginDir |
	pluginDir := Smalltalk vm binary parent / 'Plugins'.
	#('libgit2.dylib' 'libgit2.0.dylib')
		detect: [ :each | (pluginDir / each) exists ] 
		ifFound: [ :libName | ^ libName ].

	self error: 'Module not found.'
]

{ #category : #operations }
LGitLibrary >> recordInitializationFailure [

	initialized := false
]

{ #category : #operations }
LGitLibrary >> recordInitializationSuccess [

	initialized ifNil: [initialized := true]
]

{ #category : #operations }
LGitLibrary >> shutdownLibGit2 [
	[ self isInitialized
		ifTrue: [ self libgit2_shutdown ] ]
		ensure: [ self forgetInitializationState ]
]

{ #category : #'accessing platform' }
LGitLibrary >> unixModuleName [
	| pluginDir |
	pluginDir := Smalltalk vm binary parent.
	#('libgit2.so' 'libgit2.so.0')
		detect: [ :each | (pluginDir / each) exists ] 
		ifFound: [ :libName | ^ libName ].

	self error: 'Module not found.'
]

{ #category : #accessing }
LGitLibrary >> version [
	"self uniqueInstance version"

	| major minor rev |
	
	major := LGitWriteBackValueInt new.
	minor := LGitWriteBackValueInt new.
	rev := LGitWriteBackValueInt new.
	
	self libgit2_version: major minor: minor rev: rev.
	^Array with: major value with: minor value with: rev value
]

{ #category : #'accessing platform' }
LGitLibrary >> win32ModuleName [
	^ 'libgit2.dll'
]
