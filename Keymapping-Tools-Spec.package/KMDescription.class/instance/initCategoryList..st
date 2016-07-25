initialization
initCategoryList: aCollection
	categoryList emptyList.
	aCollection do: [ :cat | categoryList addItemLabeled: cat name do: [ self updateShortcutList ] ]