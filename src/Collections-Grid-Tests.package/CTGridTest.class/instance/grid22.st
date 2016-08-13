old setup
grid22
	"self new grid22"
	grid1 := self gridClass new: 2.
	grid1 atRow: 1 atColumn: 1 put: 1.
	grid1 atRow: 1 atColumn: 2 put: 3.
	grid1 atRow: 2 atColumn: 1 put: 2.
	grid1 atRow: 2 atColumn: 2 put: 4.
	^ grid1 