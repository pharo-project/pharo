accessing
inverseTransformation
	"Return the inverse transformation of the receiver"
	^self species new
		globalTransform: localTransform inverseTransformation
		localTransform: globalTransform inverseTransformation