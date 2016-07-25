public - string reading
changeRecordsFrom: initialSourcePointer className: theNonMetaClassName isMeta: classIsMeta
	"Answer the ChangeRecords of a method or class comment, starting from the initialSourcePointer. 
	Most recent changes go first."
	
	| changeRecords |
	changeRecords := OrderedCollection new.

	self
		changeRecordsFrom: initialSourcePointer
		className: theNonMetaClassName
		isMeta: classIsMeta
		do: [ :changeRecord | changeRecords add: changeRecord ].
	
	^ changeRecords