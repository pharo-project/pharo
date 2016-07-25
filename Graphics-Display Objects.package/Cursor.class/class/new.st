instance creation
new

	^ self extent: 16 @ 16
		fromArray: (Array new: 16 withAll: 0)
		offset: 0 @ 0

	"Cursor new bitEdit show"