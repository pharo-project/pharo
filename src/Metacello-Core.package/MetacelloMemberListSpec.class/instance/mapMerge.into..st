private
mapMerge: aMemberSpec into: map
	| spec |
	spec :=  map at: aMemberSpec name ifAbsent: [].
	spec == nil
		ifTrue: [ map at: aMemberSpec name put: aMemberSpec spec copy ]
		ifFalse: [ map at: aMemberSpec name put: (spec mergeSpec: aMemberSpec spec)]