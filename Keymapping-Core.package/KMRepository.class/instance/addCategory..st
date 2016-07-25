associating
addCategory: aCategory
	(self includesCategory: aCategory)
		ifTrue: [ self error: 'a Category named ' , aCategory name, ' already exists.' ].
	self categories at: aCategory name put: aCategory.