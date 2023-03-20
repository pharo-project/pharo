Extension { #name : #SystemOrganizer }

{ #category : #'*CodeExport' }
SystemOrganizer >> fileOut [
	"SystemOrganization fileOut"

	| internalStream |
	internalStream := (String new: 30000) writeStream.
	internalStream nextPutAll: 'SystemOrganization changeFromCategorySpecs: #('; cr;
		print: SystemOrganization;  "ends with a cr"
		nextPutAll: ')!'; cr.

	CodeExporter
		writeSourceCodeFrom: internalStream
		baseName: 'SystemOrganization.st' asFileReference nextVersion basenameWithoutExtension
		isSt: true
]

{ #category : #'*CodeExport' }
SystemOrganizer >> fileOutCategory: packageName [
	"Store on the file named category (a string) concatenated with '.st' all the
	classes associated with the category."

	| internalStream |
	internalStream := (String new: 1000) writeStream.
	self fileOutCategory: packageName on: internalStream.
	^ CodeExporter writeSourceCodeFrom: internalStream baseName: packageName isSt: true
]

{ #category : #'*CodeExport' }
SystemOrganizer >> fileOutCategory: packageName on: aFileStream [
	"Store on the file associated with aFileStream, all the classes associated
	with the category and any requested shared pools."
	^self fileOutCategory: packageName on: aFileStream initializing: true
]

{ #category : #'*CodeExport' }
SystemOrganizer >> fileOutCategory: packageName on: aFileStream initializing: aBool [
	"Store on the file associated with aFileStream, all the traits and classes associated
	with the category and any requested shared pools in the right order."

	| poolSet tempClass classes traits |
	traits := self orderedTraitsIn: packageName.
	classes := self superclassOrder: packageName.
	poolSet := Set new.
	classes do:  [:class | class sharedPools do: [:eachPool | poolSet add: eachPool]].
	poolSet notEmpty ifTrue: [
		tempClass := Class new.
		tempClass shouldFileOutPools ifTrue: [
			poolSet := poolSet select: [:aPool |
				tempClass shouldFileOutPool: (self environment keyAtIdentityValue: aPool)].
			poolSet do: [:aPool | tempClass fileOutPool: aPool onFileStream: aFileStream]]].
	traits, classes
		do: [:each |
			each
				fileOutOn: aFileStream
				initializing: false ]
		separatedBy: [ aFileStream cr; nextPut: Character newPage; cr ].
	aBool ifTrue: [classes do: [:cls | cls fileOutInitializerOn: aFileStream]]
]
