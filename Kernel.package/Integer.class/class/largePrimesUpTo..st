prime numbers
largePrimesUpTo: maxValue
	"Compute and return all the prime numbers up to maxValue"
	^Array streamContents:[:s| self largePrimesUpTo: maxValue do:[:prime| s nextPut: prime]]