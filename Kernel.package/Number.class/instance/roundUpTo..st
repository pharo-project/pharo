truncation and round off
roundUpTo: aNumber 
	"Answer the next multiple of aNumber toward infinity that is nearest the receiver.
	Examples:
               3.1479 roundUpTo: 0.01 -> 3.15
               3.1479 roundUpTo: 0.1 -> 3.2
               1923 roundUpTo: 10 -> 1930
               3.1479 roundUpTo: 0.005 -> 3.15
               -3.1479 roundUpTo: 0.01 -> -3.14"

	^(self / aNumber) ceiling * aNumber