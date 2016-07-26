private
cannotReturn: result to: aContext
	"The receiver tried to return result to homeContext that no longer exists."

	^ ContextCannotReturn result: result to: aContext