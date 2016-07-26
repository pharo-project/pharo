private
iconFor: rowIndex
	iconBlock ifNil: [ ^ nil ].
	^ iconBlock value: (self elementAt: rowIndex)