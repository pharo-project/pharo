testing
bucketTest: randy
	"Execute this:   Random bucketTest: Random new"
	" A quick-and-dirty bucket test. Prints nbuckets values on the
Transcript.
	  Each should be 'near' the value of ntries. Any run with any value
'far' from ntries
	  indicates something is very wrong. Each run generates different
values.
	  For a slightly better test, try values of nbuckets of 200-1000 or
more; go get coffee.
	  This is a poor test; see Knuth.   Some 'OK' runs:
		1000 1023 998 969 997 1018 1030 1019 1054 985 1003
		1011 987 982 980 982 974 968 1044 976
		1029 1011 1025 1016 997 1019 991 954 968 999 991
		978 1035 995 988 1038 1009 988 993 976
"
	| nbuckets buckets ntrys |
	nbuckets := 20.
	buckets := Array new: nbuckets.
	buckets atAllPut: 0.
	ntrys :=  100.
	ntrys*nbuckets timesRepeat: [ | slot |
		slot := (randy next * nbuckets) floor + 1.
		buckets at: slot put: (buckets at: slot) + 1 ].
	Transcript cr.
	1 to: nbuckets do: [ :nb |
		Transcript show: (buckets at: nb) printString, ' ' ]