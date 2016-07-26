ui requests
request: queryString initialAnswer: defaultAnswer
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."


	 ^ self 
		request: queryString 
		initialAnswer: defaultAnswer 
		title: 'Provide the following information'
