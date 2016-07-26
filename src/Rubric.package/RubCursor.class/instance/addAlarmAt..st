adding
addAlarmAt: millisecs
	self removeAlarm.
	(self stayVisible not and: [ self textArea hasFocus ])
		ifFalse: [ ^ self ].
	self world
		ifNotNil: [ :wld | 
			wld
				addAlarm: #hideShow
				withArguments: #()
				for: self
				at: Time millisecondClockValue + millisecs ]