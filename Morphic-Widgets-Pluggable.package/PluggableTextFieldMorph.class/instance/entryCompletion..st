accessing
entryCompletion: anEntryCompletion
	entryCompletion := anEntryCompletion.
	entryCompletion 
		ifNotNil: [ entryCompletion chooseBlock isNil
			ifTrue: [entryCompletion chooseBlock: [:v | self setText: v. self acceptTextInModel ]]
			ifFalse: [ | blk | 
				blk := entryCompletion chooseBlock.
				entryCompletion chooseBlock: [:v | self setText: v. self acceptTextInModel. blk value: v]]].
