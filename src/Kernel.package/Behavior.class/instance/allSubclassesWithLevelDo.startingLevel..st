accessing class hierarchy
allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level 
	"Walk the tree of subclasses, giving the class and its level"
	| subclassNames |
	classAndLevelBlock value: self value: level.
	self == Class ifTrue:  [^ self].  "Don't visit all the metaclasses"
	"Visit subclasses in alphabetical order"
	subclassNames := SortedCollection new.
	self subclassesDo: [:subC | subclassNames add: subC name].
	subclassNames do:
		[:name | (self environment at: name)
			allSubclassesWithLevelDo: classAndLevelBlock
			startingLevel: level+1]