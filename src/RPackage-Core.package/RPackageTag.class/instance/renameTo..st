accessing
renameTo: newTagName
	| oldName categoryName tagName |
	tagName := self name.
	oldName := self toCategoryName: tagName.
	categoryName := self toCategoryName: newTagName. 
	
	oldName = categoryName ifTrue: [ ^ self ].
	
	self basicRenameTo: newTagName.
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self classes do: [ :each | each category: categoryName ].
		self class environment organization 
			renameCategory: oldName 
			toBe: categoryName].
	SystemAnnouncer uniqueInstance 
		classTagRenamed: tagName 
		inPackage: self package 
		to: newTagName