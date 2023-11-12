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

	| toExport traits |
	traits := (self classes select: [ :class | class isTrait ]) ifNotEmpty: [ :collection | "This dependency should be managed better in the future :("
		          (self class environment at: #Trait) traitDependencyOrder: collection ].

	toExport := traits asArray , (Class superclassOrder: (self classes reject: [ :class | class isTrait ])) asArray.

	(toExport flatCollect: [ :class | class sharedPools ] as: Set) ifNotEmpty: [ :poolSet |
		self class shouldFileOutPools ifTrue: [
			poolSet
				select: [ :aPool | self class shouldFileOutPool: (self environment keyAtIdentityValue: aPool) ]
				thenDo: [ :aPool | self class fileOutPool: aPool onFileStream: aFileStream ] ] ].
	toExport do: [ :each | each fileOutOn: aFileStream initializing: false ] separatedBy: [
		aFileStream
			cr;
			nextPut: Character newPage;
			cr ].
	toExport do: [ :cls | cls fileOutInitializerOn: aFileStream ]
]
