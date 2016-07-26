testing
beginsWith: aSequenceableCollection
	"Answer true if the receiver starts with the argument collection"
	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	aSequenceableCollection withIndexDo: [:each :index | (self at: index) ~= each ifTrue: [^false]].
	^true