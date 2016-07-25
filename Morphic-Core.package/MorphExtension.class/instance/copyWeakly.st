connectors-copying
copyWeakly
	"list of names of properties whose values should be weak-copied when veryDeepCopying a morph.  See DeepCopier."

	^ #(formerOwner)
	"add yours to this list" 

	"formerOwner should really be nil at the time of the copy, but this will work just fine."