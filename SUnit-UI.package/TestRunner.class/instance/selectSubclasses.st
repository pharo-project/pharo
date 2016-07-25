accessing-classes
selectSubclasses
	"Fixed to update all selections now that the
	selection invalidation has been optimised."
	
	| classesForPackages |
	classesForPackages := self findClassesForPackages: packagesSelected.	
	classesSelected := (classesSelected flatCollect: [ :class |
		class withAllSubclasses select: [ :each |
			classesForPackages includes: each ] ])
		asSet.
	self
		changed: #allSelections;
		changed: #classSelected;
		changed: #hasRunnable