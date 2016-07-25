accessing-list
listSelection: anObject

	self listIndex: (listValues indexOf: anObject ifAbsent: [ 0 ])