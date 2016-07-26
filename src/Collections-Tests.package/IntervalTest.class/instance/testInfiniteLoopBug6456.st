tests
testInfiniteLoopBug6456
	"This is a non regression test against mantis bug #6456.
	Some Float interval size was not consistent with do: loop.
	Some Float Interval used to do: infinite loops"

	| x interval counter size |
	x := (1.0 timesTwoPower: Float precision). "Note: x+1 = x due to inexact arithmetic"
	interval := x to: x+4.
	size := interval size.
	counter := 0.
	interval do: [:each | self assert: (counter := counter + 1) <= size]
