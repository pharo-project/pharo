accessing
traitNames
	"Assuming that traits in a composition can be identified by
	testing for the first character being an uppercase character
	(and thus not a special character such as {, # etc.)"
	| tokens |
	
	tokens := self traitCompositionSource parseLiterals flattened.
	^tokens select: [:each | each first isUppercase].