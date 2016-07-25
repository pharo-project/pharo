plugins announcements
browseSuperclass

	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class |
			self browseSuperclassOf: class ]