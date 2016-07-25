class tags
classTagForClass: aClass 
	^ self 
		classTagNamed: (self toTagName: aClass category) 
		ifAbsent: [ nil ]