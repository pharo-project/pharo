testing
sortClassesInCachedHierarchy: aMethodDefinition b: otherMethodDefinition
	"This method checks wether the cached inheritance hierarchy of a method should be before than the one of another method.
	It compares alphabetically the hierarchy using #compare:
	If both are alphabetically equals, it uses the size the hierarchy.
	We do not know why this is done like this."
	| aMethodHierarchy otherMethodHierarchy minSize|
	aMethodHierarchy := cachedHierarchy at: aMethodDefinition.
	otherMethodHierarchy := cachedHierarchy at: otherMethodDefinition.
	
	minSize := aMethodHierarchy size min: otherMethodHierarchy size.
	
	1 to: minSize do: [ :i | |compare|
		compare := (aMethodHierarchy at: i) printString compare: (otherMethodHierarchy at: i) printString.
		compare ~~ 2
			ifTrue: [  ^ compare == 1 ]].
	^  aMethodHierarchy size < otherMethodHierarchy size