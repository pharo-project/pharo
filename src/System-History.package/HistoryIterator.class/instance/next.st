accessing
next
	"next item in history"
	^ self hasNext
		ifTrue: [ index := self index + 1.
			self current]