ui requests
request: queryString initialAnswer: defaultAnswer
	"Create an instance of me whose question is queryString with the given initial answer. Answer the string the user accepts. Answer the empty string if the user cancels."

	^self subclassResponsibility