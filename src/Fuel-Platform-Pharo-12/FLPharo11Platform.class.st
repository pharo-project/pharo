Class {
	#name : #FLPharo11Platform,
	#superclass : #FLPharoPlatform,
	#category : #'Fuel-Platform-Pharo-12'
}

{ #category : #operations }
FLPharo11Platform class >> addHacks [
	"no hacks for development platform"
]

{ #category : #testing }
FLPharo11Platform class >> isResponsibleForCurrentPlatform [

	^ super isResponsibleForCurrentPlatform 
		and: [ SystemVersion current type = 'Pharo' 
		and: [ SystemVersion current major = 11 ] ]
]

{ #category : #'accessing-files' }
FLPharo11Platform >> createTemporaryDirectoryPathNamed: aString [
	^ (FileLocator temp / aString) ensureCreateDirectory pathString
]

{ #category : #'accessing-files' }
FLPharo11Platform >> deleteAllInDirectoryPath: aPathString [
	aPathString asFileReference deleteAll
]

{ #category : #'accessing-files' }
FLPharo11Platform >> deleteFilePath: aFilename [
	((Smalltalk at: #File) named: aFilename) delete
]

{ #category : #'tests-testing' }
FLPharo11Platform >> expectedFailuresOf: aClass [
	| inherited |
	inherited := super expectedFailuresOf: aClass.
	(aClass includesBehavior: (Smalltalk at: #FLBasicSerializationTest)) ifTrue: [
		inherited addAll: #(
			"Global values are expected to be of type String. We'd need to modify string
			encoding to support String and WideString or treat the value as an arbitrary
			object and let the engine decide how to serialize."
			testWideStringGlobal
			testConsiderCustomWideSymbolGlobal
			testWideStringClassName) ].

	(FLVersion current major <= 5 and: [
		FLVersion current minor <= 1 ]) ifTrue: [
			(aClass includesBehavior: (Smalltalk at: #FLGlobalEnvironmentTest)) ifTrue: [
				inherited addAll: #(
					testCompiledMethodChanged) ].
			(aClass includesBehavior: (Smalltalk at: #FLBlockClosureSerializationTest)) ifTrue: [
				inherited addAll: #(
					testBlockClosureChangeDifferentBytecodes
					testBlockClosureChangeSameBytecodes
					testBlockClosureRemoved) ].
			(aClass includesBehavior: (Smalltalk at: #FLFullBlockClosureSerializationTest)) ifTrue: [
				inherited addAll: #(
					testBlockClosureChangeDifferentBytecodes) ] ].
		
	^ inherited
]

{ #category : #'accessing-files' }
FLPharo11Platform >> filePathExists: aPathString [
	^ aPathString asFileReference exists
]

{ #category : #testing }
FLPharo11Platform >> isBigEndian [
	^ (Smalltalk at: #EndianDetector) isBigEndian
]

{ #category : #'accessing-files' }
FLPharo11Platform >> joinPath: aPathString with: anotherPathString [
	^ (aPathString asFileReference / anotherPathString) pathString
]

{ #category : #'accessing-files' }
FLPharo11Platform >> readStreamForFilePath: aString [
	^ ((Smalltalk at: #File) named: aString) readStream
		binary;
		yourself
]

{ #category : #'accessing-compiler' }
FLPharo11Platform >> silentlyAddAndClassifySelector: aSymbol inClass: aClass withMethod: aCompiledMethod inProtocol: aString [

	aClass addSelector: aSymbol withMethod: aCompiledMethod.
	aClass classify: aSymbol under: aString
]

{ #category : #'accessing-kernel' }
FLPharo11Platform >> withPragmasIn: aClass do: aBlock [
	aClass pragmasDo: aBlock
]

{ #category : #'accessing-files' }
FLPharo11Platform >> writeStreamForFilePath: aString [
	^ ((Smalltalk at: #File) named: aString) writeStream
		binary;
		yourself
]
