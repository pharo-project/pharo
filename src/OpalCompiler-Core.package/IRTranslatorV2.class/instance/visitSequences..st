visiting
visitSequences:  irSequences
	irSequences withIndexDo: [ :seq :i | seq orderNumber: i].
	self visitNodes: irSequences.
