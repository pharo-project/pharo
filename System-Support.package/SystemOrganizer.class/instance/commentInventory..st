queries
commentInventory: categoryName
	"SystemOrganization commentInventory: 'Morphic*'"

	| classes commentedClasses |
	classes := OrderedCollection new.
	self categories
		withIndexCollect: [ :cat :idx | 
			(categoryName match: cat)
				ifTrue: [ classes addAll: (self listAtCategoryNumber: idx) ]
				ifFalse: [ nil ] ].
	commentedClasses := classes select: [ :catCls | (self environment at: catCls) hasComment ].
	^ 'There are ' , classes size asString , ' classes in ' , categoryName , ' of which ' , commentedClasses size asString
		, ' have comments and ' , (classes size - commentedClasses size) asString , ' do not yet have comments.'