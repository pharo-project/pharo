updating
updateContentFromSession: aSession
	
	self list items: (aSession 
		ifNil: [ OrderedCollection new ] 
		ifNotNil: [ self filteredStackOfSize: self class defaultStackSize ] )