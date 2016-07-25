accessing-list
listSelection
	^ listValues at: self listIndex ifAbsent: [ nil ]