accessing
renameTo: aString 
	| oldName categoryName |
	
	oldName := self toCategoryName: self name.
	categoryName := self toCategoryName: aString. 
	
	oldName = categoryName ifTrue: [ ^ self ].
	
	self basicRenameTo: aString.
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self classes do: [ :each | each category: categoryName ].
		self class environment organization 
			renameCategory: oldName 
			toBe: categoryName. ].