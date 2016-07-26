accessing
addClass: aClass
	"TODO: deprecate system category / replace this with a direct call to moveClass:fromPackage:toTag:"
	
	"This does two things:
		- make sure the class tag for ourselves exists
		- change the class category as appropriate.
			(and by cascade, ensure systemClassRecategorizedAction: is called)."
	
	aClass category: (self addClassTag: self categoryName) categoryName