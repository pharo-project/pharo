simple symbol tags
tagsForClasses
	"Any class could be tagged for user purpose.
	Now we implement it on top of RPackageTag"
	
	^self classTags reject: #isRoot thenCollect: #name