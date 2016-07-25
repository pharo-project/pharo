private
createRootItem
	| fullRoot |
	fullRoot := FTRootItem data: self basicRoots from: self.
	^ self tagsToFilterBy
		ifEmpty: [ fullRoot ]
		ifNotEmpty:
			[ | children |
			children := OrderedCollection new.
			fullRoot children
				do: [ :item | (self matchingTag: item) ifNotNil: [ :itemNew | children add: itemNew ] ]
				displayingProgress: [ :each | 'Looking inside ' , each name ].
			FTRootItem new
				dataSource: self;
				children: children asArray;
				yourself ]