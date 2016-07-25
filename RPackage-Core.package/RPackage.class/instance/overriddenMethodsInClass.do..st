system compatibility
overriddenMethodsInClass: aClass do: aBlock
	"Evaluates aBlock with the overridden methods in aClass"
	^ self overrideCategoriesForClass: aClass do: [:cat |
		self methodsInCategory: cat ofClass: aClass do: aBlock]