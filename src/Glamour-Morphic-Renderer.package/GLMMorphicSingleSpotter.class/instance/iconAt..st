callbacks
iconAt: anIndex
	^ model ifNotNil: [model iconFor: (listValues at: anIndex ifAbsent: [ ^ nil ])]