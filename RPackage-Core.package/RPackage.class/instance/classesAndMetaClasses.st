system compatibility
classesAndMetaClasses
	"Return a Set with all classes and metaclasses belonging to this package"
	self flag: #stef. "only used by goferCleanUp"
	^ OrderedCollection new
		addAll: self definedClasses;
		addAll: (self definedClasses collect: [:each | each classSide]);
		yourself.