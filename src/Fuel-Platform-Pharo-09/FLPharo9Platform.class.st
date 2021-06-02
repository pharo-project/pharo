Class {
	#name : #FLPharo9Platform,
	#superclass : #FLPharoPlatform,
	#category : #'Fuel-Platform-Pharo-09'
}

{ #category : #operations }
FLPharo9Platform class >> addHacks [
	"no hacks for development platform"
]

{ #category : #testing }
FLPharo9Platform class >> isResponsibleForCurrentPlatform [
	^ super isResponsibleForCurrentPlatform and: [
		(SystemVersion current type = 'Pharo')
			and: [ SystemVersion current major = 9 ] ]
]

{ #category : #'accessing-compiler' }
FLPharo9Platform >> addAndClassifySelector: aSymbol inClass: aClass withMethod: aCompiledMethod inProtocol: aString [
	aClass
		addAndClassifySelector: aSymbol
		withMethod: aCompiledMethod
		inProtocol: aString
]

{ #category : #'accessing-files' }
FLPharo9Platform >> deleteFileNamed: aFilename [
	((Smalltalk at: #File) named: aFilename) delete
]

{ #category : #'accessing-files' }
FLPharo9Platform >> fileNamed: aFilename readStreamDo: aBlock [
	^ ((Smalltalk at: #File) named: aFilename) readStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]
]

{ #category : #'accessing-files' }
FLPharo9Platform >> fileNamed: aFilename writeStreamDo: aBlock [
	^ ((Smalltalk at: #File) named: aFilename) writeStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]
]

{ #category : #testing }
FLPharo9Platform >> isBigEndian [
	^ (Smalltalk at: #EndianDetector) isBigEndian
]

{ #category : #'accessing-compiler' }
FLPharo9Platform >> silentlyAddAndClassifySelector: aSymbol inClass: aClass withMethod: aCompiledMethod inProtocol: aString [
	aClass
		addSelector: aSymbol
		withMethod: aCompiledMethod.
	aClass organization
		classify: aSymbol
		under: aString
]

{ #category : #'accessing-kernel' }
FLPharo9Platform >> withPragmasIn: aClass do: aBlock [
	aClass pragmasDo: aBlock
]
