accessing
findString: aString startingAt: start 
	"Answer the index of substring within the receiver, starting at index 
	start. If the receiver does not contain substring, answer 0."

	^string findString: aString asString startingAt: start