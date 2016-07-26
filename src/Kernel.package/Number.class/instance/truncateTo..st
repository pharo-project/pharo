truncation and round off
truncateTo: aNumber 
	"Answer the next multiple of aNumber toward zero that is nearest the receiver.
	Examples:
               3.1479 truncateTo: 0.01 -> 3.14
               3.1479 truncateTo: 0.1 -> 3.1
               1923 truncateTo: 10 -> 1920
               3.1479 truncateTo: 0.005 -> 3.145
               -3.1479 truncateTo: 0.01 -> -3.14"

	^(self quo: aNumber) * aNumber