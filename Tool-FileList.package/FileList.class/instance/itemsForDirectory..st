file list menu
itemsForDirectory: dir 
	| services |
	services := OrderedCollection new.
	dir ifNotNil: [
		services
			addAll: (self class itemsForDirectory: dir).
		services ifNotEmpty: [services last useLineAfter] ].
	services add: self serviceAddNewFile.
	services add: self serviceAddNewDirectory.
	services addAll: self configuredServices.
	^ services