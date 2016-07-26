accessing
previous
	"previous item in history"
	^ self hasPrevious
		ifTrue: [index := self index - 1.
			self current]