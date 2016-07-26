initialization
shortCuts
	"Answers a dictionary of the shortcuts usable in EyeInspector"

	|shortCuts|

	shortCuts := Dictionary new.
	
	shortCuts at: $d meta put: [ self diveIntoSelectedObject ].
	
	shortCuts at: $i meta put: [ self inspectSelectedObjectInNewWindow ].
	shortCuts at: $i meta shift put: [ self exploreSelectedObject ].
	
	shortCuts at: $b meta put: [ self browseSelectedObject ].
	shortCuts at: $h meta put: [ self browseSelectedObjectClassHierarchy ].
	
	^shortCuts