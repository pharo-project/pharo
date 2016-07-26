accessing
renameTo: aString category: categoryName
	| oldName |
	
	oldName := self toCategoryName: self name.
	oldName = categoryName ifTrue: [ ^ self ].
	
	self basicRenameTo: aString.
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self classes do: [ :each | each category: categoryName ].
		self class environment organization 
			renameCategory: oldName 
			toBe: categoryName. ].