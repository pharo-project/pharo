running
tearDown

	super tearDown.
	self killTestFile.
	"We must ensure that files are collected before running other tests.
	In windows, we cannot open the same file twice."
	3 timesRepeat: [ Smalltalk garbageCollect. ]