navigating
previous
	self helpBrowserWindow 
		ifNotNil: [ :window | window model previous ].
	self lessonView isOpenInWindow 
		ifTrue: [ 
			self player previous.
			self showCurrentLesson ]