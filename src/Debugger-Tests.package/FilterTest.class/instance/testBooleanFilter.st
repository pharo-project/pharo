tests
testBooleanFilter
	| kernelFilter doItFilter filters booleanFilter |
	
	kernelFilter := KernelClassesFilter new.
	doItFilter := SelectorFilter forSelector: #doIt.
	filters := OrderedCollection with: kernelFilter with: doItFilter.
	booleanFilter := BooleanFilter withFilters: filters operator: #or:.
	self assert: (booleanFilter shouldDisplay: context)