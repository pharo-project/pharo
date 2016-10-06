highlighting
getSegmentOfClass: aClass

	^ self segments detect: [ :each | each class = aClass ] ifNone: [ nil ]