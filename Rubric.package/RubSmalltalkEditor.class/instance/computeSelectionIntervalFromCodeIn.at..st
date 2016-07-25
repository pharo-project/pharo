private
computeSelectionIntervalFromCodeIn: aString at: anInterval
	^ (RBParser parseExpression: (aString copyFrom: anInterval first to: anInterval last)) sourceInterval