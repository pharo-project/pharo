literals
literalAt: index put: value 
	"Replace the literal indexed by the first argument with the second 
	argument. Answer the second argument."

	^self objectAt: index + 1 put: value