accessing
collectMisplacedItemsIn: aCollection
	"Select recursively all MenuRegistration with a parentName 
	different from its owner name. 	This is possible since a parent name 
	can be given in order to force a menu item placement. 
	If my parentName is not the same as my owner name then it means 
	that I'm not in the right place and that my placement has to be resolved"
	
	self parentName
		ifNotNil: [owner
				ifNil: [aCollection add: self]
				ifNotNil: [owner name = self parentName
						ifFalse: [aCollection add: self]]].
	self itemList ifNotNil: [:l |
		l do: [:item | item collectMisplacedItemsIn: aCollection]]