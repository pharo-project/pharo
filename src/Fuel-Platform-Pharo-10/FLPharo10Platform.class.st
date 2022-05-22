Class {
	#name : #FLPharo10Platform,
	#superclass : #FLPharoPlatform,
	#category : #'Fuel-Platform-Pharo-10'
}

{ #category : #operations }
FLPharo10Platform class >> addHacks [
	"no hacks for development platform"
]

{ #category : #testing }
FLPharo10Platform class >> isResponsibleForCurrentPlatform [
	^ super isResponsibleForCurrentPlatform and: [
		(SystemVersion current type = 'Pharo')
			and: [ SystemVersion current major = 10 ] ]
]

{ #category : #'accessing - compiler' }
FLPharo10Platform >> addAndClassifySelector: aSymbol inClass: aClass withMethod: aCompiledMethod inProtocol: aString [
	aClass
		addAndClassifySelector: aSymbol
		withMethod: aCompiledMethod
		inProtocol: aString
]

{ #category : #'accessing - files' }
FLPharo10Platform >> deleteFileNamed: aFilename [
	((Smalltalk at: #File) named: aFilename) delete
]

{ #category : #'accessing - files' }
FLPharo10Platform >> fileNamed: aFilename readStreamDo: aBlock [
	^ ((Smalltalk at: #File) named: aFilename) readStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]
]

{ #category : #'accessing - files' }
FLPharo10Platform >> fileNamed: aFilename writeStreamDo: aBlock [
	^ ((Smalltalk at: #File) named: aFilename) writeStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]
]

{ #category : #testing }
FLPharo10Platform >> isBigEndian [
	^ (Smalltalk at: #EndianDetector) isBigEndian
]

{ #category : #'accessing - compiler' }
FLPharo10Platform >> silentlyAddAndClassifySelector: aSymbol inClass: aClass withMethod: aCompiledMethod inProtocol: aString [
	aClass
		addSelector: aSymbol
		withMethod: aCompiledMethod.
	aClass organization
		classify: aSymbol
		under: aString
]

{ #category : #'accessing - kernel' }
FLPharo10Platform >> withPragmasIn: aClass do: aBlock [
	aClass pragmasDo: aBlock
]
