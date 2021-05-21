Class {
	#name : #FLPharoPlatform,
	#superclass : #FLPlatform,
	#category : #'Fuel-Platform-Pharo-Core'
}

{ #category : #'private-extensions-znbufferedreadstream' }
FLPharoPlatform class >> fuelNextWordsInto: aWordObject [
	<flExtensionOf: #ZnBufferedReadStream>
	"This method is the same as nextWordsInto: but the restoreEndianness is only done if needed"
	| byteSize source pos blt |
	byteSize := aWordObject byteSize.
	source := self next: byteSize.
	pos := 0.

	"Now use BitBlt to copy the bytes to the bitmap."
	blt := (BitBlt
				toForm: (Form new hackBits: aWordObject))
				sourceForm: (Form new hackBits: source).
	blt combinationRule: Form over. "store"
	blt sourceX: 0;
		 sourceY: pos // 4;
		 height: byteSize // 4;
		 width: 4.
	blt destX: 0;
		 destY: 0.
	blt copyBits.
	
	^ aWordObject
]

{ #category : #testing }
FLPharoPlatform class >> isResponsibleForCurrentPlatform [
	^ SystemVersion current version beginsWith: 'Pharo'
]

{ #category : #'accessing-system' }
FLPharoPlatform >> absolutePathOf: aString [
	^ aString asFileReference fullName
]

{ #category : #'accessing-compiler' }
FLPharoPlatform >> addAndClassifySelector: aSymbol inClass: aClass withMethod: aCompiledMethod inProtocol: aString [

	aClass addAndClassifySelector: aSymbol withMethod: aCompiledMethod inProtocol: aString
]

{ #category : #'accessing-system' }
FLPharoPlatform >> imageName [
	^ Smalltalk image shortImageName
]

{ #category : #testing }
FLPharoPlatform >> isPharo [
	^ true
]

{ #category : #testing }
FLPharoPlatform >> isSpur [
	^ true
]

{ #category : #'accessing-compiler' }
FLPharoPlatform >> renamedMethodCopyOf: aMethod selector: aSymbol [
	| newMethod |
	newMethod := aMethod ast
		selector: aSymbol
		yourself.
	"#compiledMethod answers a new instance"
	^ newMethod compiledMethod
]

{ #category : #'accessing-tests' }
FLPharoPlatform >> setTestTimeLimitFor: aTest to: aDuration [
	aTest timeLimit: aDuration
]

{ #category : #'accessing-system' }
FLPharoPlatform >> useAuthor: aString during: aBlock [
	(Smalltalk at: #Author)
		useAuthor: aString
		during: aBlock
]
