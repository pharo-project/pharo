examples
grid6x2
	"self grid6x2"
	| grid62 |
	grid62 := self columns: 6 rows: 2.
	grid62 atX: 1 atY: 1 put: 11.
	grid62 atX: 2 atY: 1 put: 21.
	grid62 atX: 3 atY: 1 put: 31.
	grid62 atX: 4 atY: 1 put: 41.
	grid62 atX: 5 atY: 1 put: 51.
	grid62 atX: 6 atY: 1 put: 61.
	grid62 atX: 1 atY: 2 put: 12.
	grid62 atX: 2 atY: 2 put: 22.
	grid62 atX: 3 atY: 2 put: 32.
	grid62 atX: 4 atY: 2 put: 42.
	grid62 atX: 5 atY: 2 put: 52.
	grid62 atX: 6 atY: 2 put: 62.
	^ grid62