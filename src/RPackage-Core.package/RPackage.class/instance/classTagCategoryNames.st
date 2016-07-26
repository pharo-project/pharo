class tags
classTagCategoryNames
	^ (Set with: self packageName), (self classTags collect: #categoryName).