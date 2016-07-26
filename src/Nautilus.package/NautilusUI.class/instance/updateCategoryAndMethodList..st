updating
updateCategoryAndMethodList: aMethodOrNil
	self resetCategoriesList.
	categoryWidget updateCategoryList.
	self resetMethodsList.
	methodWidget updateMethodList: aMethodOrNil