accessing
qTable
	qTable ifNil: [ qTable := Array new: QuantizationTableSize ].
	^ qTable