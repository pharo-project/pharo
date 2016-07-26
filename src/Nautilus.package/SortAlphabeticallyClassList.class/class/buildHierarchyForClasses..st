instance creation
buildHierarchyForClasses: aCollection
	| nodes |
	nodes := aCollection asOrderedCollection sort: [:a :b  | a name < b name].
	^ self new
		list: nodes;
		yourself