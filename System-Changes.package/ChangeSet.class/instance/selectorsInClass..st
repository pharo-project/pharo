method changes
selectorsInClass: aClassName
	"Used by a ChangeSorter to access the list methods."

	^ (changeRecords at: aClassName ifAbsent: [^#()]) changedSelectors