*CodeExport
fileOutCategory: category on: aFileStream initializing: aBool
	"Store on the file associated with aFileStream, all the traits and classes associated 
	with the category and any requested shared pools in the right order."

	| poolSet tempClass classes traits |
	traits := self orderedTraitsIn: category.
	classes := self superclassOrder: category.
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
	aBool ifTrue: [classes do: [:cls | cls fileOutInitializerOn: aFileStream]].