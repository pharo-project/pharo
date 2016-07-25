adding
repeatLastIfEmpty: defaultBlock
	"add the last value back again. If we are empty, add (defaultBlock value)"
	lastIndex := nil. "flush access cache"
	(runs size=0)
		ifTrue:[
			runs := runs copyWith: 1.
			values := values copyWith: defaultBlock value]
		ifFalse:
			[runs at: runs size put: runs last+1]