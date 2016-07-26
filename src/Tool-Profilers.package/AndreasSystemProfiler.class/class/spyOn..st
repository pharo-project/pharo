spying
spyOn: aBlock
	"The main method for profiling and showing the report "

	"self spyOn: [ 10000 timesRepeat: [ 3.14159 printString ] ]"

	| profiler |
	profiler := self new.
	^ [ profiler spyOn: aBlock ]
		ensure: [ profiler doReport ]