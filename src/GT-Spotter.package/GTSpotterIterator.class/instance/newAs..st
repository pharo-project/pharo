private
newAs: aCollectionClass
	^ items 
		ifNil: [ aCollectionClass new ] 
		ifNotNil: [ aCollectionClass new: self size ]