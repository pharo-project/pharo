accessing
realValue: anObject
	"Set the value of realValue"
	self settingReceiver perform: self setSelector with: anObject.
	self triggerEvent: self privateChangeEvent with: self.