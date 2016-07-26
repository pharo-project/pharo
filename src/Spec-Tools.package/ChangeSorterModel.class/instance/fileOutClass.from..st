class
fileOutClass: aClass from: aChangeSet
	"this is a hack!! makes a new change set, called the class name, adds author initials to try to make a unique change set name, files it out and removes it. kfr 16 june 2000"
	"Method copied from ChangeSorter"
	
	| aSet |
      aSet := ChangeSet newChangeSet: aClass name.
	aSet ifNil:[ ^self ].
	aSet absorbClass: aClass name from: aChangeSet.
	aSet fileOut.
	ChangeSet removeChangeSet: aSet.