accessing
selectedClassWithoutChangingSelectionInternally: aClass
	| package packageChanged |
	package := self selectedPackage.
	self showHierarchy
		ifTrue: [ aClass ifNotNil: [ package := aClass package ] ].
	aClass ifNil: [ self resetClassesListSelection ].
	aClass ifNotNil: [ self giveFocusTo: self classWidget ].
	packageChanged := package ~= self selectedPackage.
	self classWidget selectClass: aClass.
	self model selectedClass: aClass.
	self setWindowTitle.
	packageChanged
		ifTrue:
			[ 
			self resetPackageGroupsListSelection.
			self packageWidget selectPackage: package.
			self updatePackageGroupAndClassList ].
	self resetCategoriesListSelection.
	self resetMethodsListSelection.
	self updateCategoryAndMethodList.
	self changed: #isAClassSelected