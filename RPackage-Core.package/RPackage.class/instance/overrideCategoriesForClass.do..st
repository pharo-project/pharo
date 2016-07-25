system compatibility
overrideCategoriesForClass: aClass do: aBlock
	"Evaluates aBlock with all the *foo-override categories in aClass"
	^ aClass organization categories do: [:cat |
		(self isOverrideCategory: cat) ifTrue: [aBlock value: cat]]