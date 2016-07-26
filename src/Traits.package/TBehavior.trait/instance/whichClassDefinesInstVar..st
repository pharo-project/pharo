queries
whichClassDefinesInstVar: aString 
	^self 
		whichSuperclassSatisfies: [:aClass | aClass instVarNames includes: aString]