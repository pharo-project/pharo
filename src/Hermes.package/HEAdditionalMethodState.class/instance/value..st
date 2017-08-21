accessing
value: anAdditionalMethodState
	method := anAdditionalMethodState method asExportedLiteral.
	selector := anAdditionalMethodState selector asExportedLiteral.

	value := Array new: anAdditionalMethodState size.
	1 to: anAdditionalMethodState size do: [ :idx | value at: idx put: (anAdditionalMethodState basicAt: idx) ].
	value := value asExportedLiteral