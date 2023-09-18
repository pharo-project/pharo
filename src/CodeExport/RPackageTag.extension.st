Extension { #name : 'RPackageTag' }

{ #category : '*CodeExport' }
RPackageTag >> fileOut [

	| internalStream |
	internalStream := (String new: 1000) writeStream.

	self fileOutOn: internalStream.

	^ CodeExporter writeSourceCodeFrom: internalStream baseName: self categoryName isSt: true
]

{ #category : '*CodeExport' }
RPackageTag >> fileOutOn: aFileStream [
	"Store on the file associated with aFileStream, all the traits and classes associated
	with the category and any requested shared pools in the right order."

	| toExport |
	toExport := Class superclassOrder: self classes.
	(toExport flatCollect: [ :class | class sharedPools ] as: Set) ifNotEmpty: [ :poolSet |
		| tempClass |
		tempClass := Class new.
		tempClass shouldFileOutPools ifTrue: [
			poolSet
				select: [ :aPool | tempClass shouldFileOutPool: (self environment keyAtIdentityValue: aPool) ]
				thenDo: [ :aPool | tempClass fileOutPool: aPool onFileStream: aFileStream ] ] ].
	toExport do: [ :each | each fileOutOn: aFileStream initializing: false ] separatedBy: [
		aFileStream
			cr;
			nextPut: Character newPage;
			cr ].
	toExport do: [ :cls | cls fileOutInitializerOn: aFileStream ]
]
