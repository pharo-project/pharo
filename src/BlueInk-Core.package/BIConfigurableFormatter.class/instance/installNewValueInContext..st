public interface
installNewValueInContext: aCol
	"self new installNewValueInContext: { (#cascadedMessageInsideParentheses: -> true) }"
	
	context := context copy.
	"needed because by default context point to the DefaultPrettyPrintContext"
	aCol do: [ :each | self perform: each key with: each value]