blinking
removeAlarm
	self world
		ifNotNil: [ :wld | 
			wld removeAlarm: #hideShow for: self ] 