instance creation simple
nanoSeconds: nanos
	"This method is slow. If you have nanos less than 10^6 you should use #seconds:nanoSeconds: instead."

	| quo |
	quo := nanos quo: NanosInSecond.
	^ self basicNew
		seconds: quo
		nanoSeconds: nanos - (quo * NanosInSecond)