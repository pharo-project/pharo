accessing
realValue: aValue
	aValue
		ifNil: [self settingReceiver perform: self setSelector with: nil]
		ifNotNil: [self settingReceiver perform: self setSelector with: ((aValue max: self range first) min: self range last)].
	self triggerEvent: self privateChangeEvent with: self
	