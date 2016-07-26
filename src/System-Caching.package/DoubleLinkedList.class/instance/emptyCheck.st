private
emptyCheck
	self isEmpty
		ifTrue: [ CollectionIsEmpty signalWith: self ]