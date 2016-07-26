ui requests
request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: anEntryCompletion
	"Create an instance of me whose question is queryString with the given 
	initial answer. Invoke it centered at the given point, and answer the 
	string the user accepts. Answer the empty string if the user cancels."
	^self subclassResponsibility