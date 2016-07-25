private
loadPackageClassListElements
	^ self sortHierarchically
		ifTrue: [ self createClassHierarchyFor: self model selectedPackageGroup ]
		ifFalse: [ self createClassListFor:self model selectedPackageGroup ]