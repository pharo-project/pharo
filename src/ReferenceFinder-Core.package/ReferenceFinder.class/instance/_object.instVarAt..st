private
_object: anObject instVarAt: anInteger
	"Answer with a fixed variable in anObject.  The numbering of the variables
	 corresponds to the named instance variables.  Fail if the index is not an
	 Integer or is not the index of a fixed variable."

	^anObject instVarAt: anInteger