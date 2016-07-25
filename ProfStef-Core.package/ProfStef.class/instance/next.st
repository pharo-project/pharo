navigating
next
	self helpBrowserWindow 
		ifNotNil: [ :window | window model next ].
	self lessonView isOpenInWindow 
		ifTrue: [ 
			self player next.
			self showCurrentLesson ]