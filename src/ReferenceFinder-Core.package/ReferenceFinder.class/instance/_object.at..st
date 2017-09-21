private
_object: anObject at: anInteger
	"Answer the value of an indexable field in anObject. Fail if the
	 argument anInteger is not an Integer or is out of bounds."

	^anObject basicAt: anInteger