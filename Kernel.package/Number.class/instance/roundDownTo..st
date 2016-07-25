truncation and round off
roundDownTo: aNumber
	"Answer the next multiple of aNumber toward negative infinity that is nearest the receiver. 
	Examples:
               3.1479 roundDownTo: 0.01 -> 3.14
               3.1479 roundDownTo: 0.1 -> 3.1
               1923 roundDownTo: 10 -> 1920
               3.1479 roundDownTo: 0.005 -> 3.145
               -3.1479 roundDownTo: 0.01 -> -3.15"
	
	^(self / aNumber) floor * aNumber