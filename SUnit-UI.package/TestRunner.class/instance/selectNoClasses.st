accessing-classes
selectNoClasses
	"Fixed to update all selections now that the
	selection invalidation has been optimised."
	
	classesSelected := Set new.
	self
		changed: #allSelections;
		changed: #classSelected;
		changed: #hasRunnable