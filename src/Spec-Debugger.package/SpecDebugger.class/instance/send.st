old - public
send
	
	self okToChange ifTrue: [  
		self session stepInto: self currentContext ]
