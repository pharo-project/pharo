settings
createSettingRow
	| list |
	^ Smalltalk ui theme 
		newRowIn: World 
		for: {
			list := self createIconPackList.
			self createFetchButtonUpdating: list }