scripting
sendOneOrMultipleAs: aCollectionClass updatedBy: oneArgBlock
	"It returns one selected item or a collection of aCollectionClass updated by oneArgBlock.
	oneArgBlock can add additional information to the collection or transform the collection."
	self send: [ :linkedListOrNil |
		linkedListOrNil ifNotNil: [ 
			linkedListOrNil size = 1
				ifTrue: [ (linkedListOrNil first isCollection and: [ linkedListOrNil first size = 1 ]) 
					ifTrue: [ linkedListOrNil first first ]
					ifFalse: [ linkedListOrNil first ] ]
				ifFalse: [ oneArgBlock cull: (linkedListOrNil as: aCollectionClass) ] ]
		 ]