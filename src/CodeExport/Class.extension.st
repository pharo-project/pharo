Extension { #name : 'Class' }

{ #category : '*CodeExport' }
Class >> fileOut [
	"Create a file whose name is the name of the receiver with '.st' as the extension, and file a description of the receiver onto it."

	| internalStream |
	internalStream := (String new: 100) writeStream.
	internalStream header; timeStamp.
	self fileOutOn: internalStream.

	CodeExporter writeSourceCodeFrom: internalStream baseName: self name isSt: true
]

{ #category : '*CodeExport' }
Class >> fileOutInitializerOn: aStream [
	^self class fileOutInitializerOn: aStream
]

{ #category : '*CodeExport' }
Class >> fileOutOn: aFileStream [
	"File a description of the receiver on aFileStream."

	^ self fileOutOn: aFileStream initializing: true
]

{ #category : '*CodeExport' }
Class >> fileOutOn: aFileStream initializing: aBool [
	"File a description of the receiver on aFileStream."

	SystemNotification signal: self name.
	super fileOutOn: aFileStream.
	self class nonTrivial
		ifTrue: [
			aFileStream cr; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!'; cr; cr.
			self class
				fileOutOn: aFileStream
				initializing: aBool ]
]
