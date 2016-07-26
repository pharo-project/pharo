accessing
realValue
	"Answer the value of realValue"

	^ (self settingReceiver perform: self getSelector) 
		ifNil: [self setToDefault. 
			self settingReceiver perform: self getSelector]