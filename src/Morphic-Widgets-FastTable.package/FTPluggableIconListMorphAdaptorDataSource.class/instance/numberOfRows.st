accessing
numberOfRows
	^ self model 
		ifNotNil: [ 
			self getListSizeSelector 
				ifNotNil: [ self model perform: self getListSizeSelector ]
				ifNil: [ (self model perform: self getListSelector) size ] ]
		ifNil: [ 0 ]