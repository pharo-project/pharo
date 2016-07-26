all changes
noteChangeType: changeSymbol fromClass: class

	(changeSymbol = #new or: [changeSymbol = #add]) ifTrue:
		[changeTypes add: #add.
		changeTypes remove: #change ifAbsent: [].
		^ self].
	changeSymbol = #change ifTrue:
		[(changeTypes includes: #add) ifTrue: [^ self].
		^ changeTypes add: changeSymbol].
	changeSymbol == #addedThenRemoved ifTrue:
		[^ self].  "An entire class was added but then removed"
	changeSymbol = #comment ifTrue:
		[^ changeTypes add: changeSymbol].
	changeSymbol = #reorganize ifTrue:
		[^ changeTypes add: changeSymbol].
	changeSymbol = #rename ifTrue:
		[^ changeTypes add: changeSymbol].
	(changeSymbol beginsWith: 'oldName: ') ifTrue:
		["Must only be used when assimilating other changeSets"
		(changeTypes includes: #add) ifTrue: [^ self].
		priorName := changeSymbol copyFrom: 'oldName: ' size + 1 to: changeSymbol size.
		^ changeTypes add: #rename].
	changeSymbol = #remove ifTrue:
		[(changeTypes includes: #add)
			ifTrue: [changeTypes add: #addedThenRemoved]
			ifFalse: [changeTypes add: #remove].
		^ changeTypes removeAllFoundIn: #(add change comment reorganize)].

	self error: 'Unrecognized changeType'