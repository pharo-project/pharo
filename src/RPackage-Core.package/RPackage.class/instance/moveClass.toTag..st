private
moveClass: aClass toTag: aTag
	"TODO: deprecate system categories / replace this with a direct call to moveClass:fromPackage:toTag:"
	
	"This change the class category as appropriate.
		(and by cascade, ensure systemClassRecategorizedAction: is called)."

	aClass category: aTag categoryName