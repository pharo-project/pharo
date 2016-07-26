submorphs-accessing
allMorphsInto: aSet
	"Return a set of all submorphs.  Don't forget the hidden ones. Consider only objects that are in memory (see allNonSubmorphMorphs)." 

	submorphs do: [:m | m allMorphsInto: aSet].
	self allNonSubmorphMorphs do: [:m | 
			(aSet includes: m) ifFalse: ["Stop infinite recursion"
				m allMorphsInto: aSet]].
	aSet add: self.
	^ aSet