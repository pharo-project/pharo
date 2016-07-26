menus behavior
restrictedBrowseClasses: classes

	| newEnvironment |
	newEnvironment := self browsedEnvironment forClasses: (classes collect:#theNonMetaClass).
	self model class 
		openOnClass:self selectedClass 
		inEnvironment: newEnvironment 