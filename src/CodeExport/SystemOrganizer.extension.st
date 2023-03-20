Extension { #name : #SystemOrganizer }

{ #category : #'*CodeExport' }
SystemOrganizer >> fileOutPackageNamed: packageName [
	"Store on the file named category (a string) concatenated with '.st' all the
	classes associated with the category."

	| internalStream |
	internalStream := (String new: 1000) writeStream.
	self fileOutPackageNamed: packageName on: internalStream.
	^ CodeExporter writeSourceCodeFrom: internalStream baseName: packageName isSt: true
]

{ #category : #'*CodeExport' }
SystemOrganizer >> fileOutPackageNamed: packageName on: aFileStream [
	"Store on the file associated with aFileStream, all the traits and classes associated
	with the package and any requested shared pools in the right order."

	| tempClass classes traits |
	traits := self orderedTraitsIn: packageName.
	classes := self superclassOrder: packageName.
	(classes flatCollectAsSet: [ :class | class sharedPools ]) ifNotEmpty: [ :poolSet |
		tempClass := Class new.
		tempClass shouldFileOutPools ifTrue: [
			poolSet
				select: [ :aPool | tempClass shouldFileOutPool: (self environment keyAtIdentityValue: aPool) ]
				thenDo: [ :aPool | tempClass fileOutPool: aPool onFileStream: aFileStream ] ] ].
	traits , classes do: [ :each | each fileOutOn: aFileStream initializing: false ] separatedBy: [
		aFileStream
			cr;
			nextPut: Character newPage;
			cr ].
	classes do: [ :cls | cls fileOutInitializerOn: aFileStream ]
]
