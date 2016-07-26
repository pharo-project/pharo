pages
testing

	^ HelpTopic
		title: 'Testing'
		contents: 
'It is fairly easy to run your rule and obtain the results. Just create an instance of it an send it the #check: message with the entity you want to check. The result is a collection of critiques. For example inspecting

	RBExcessiveMethodsRule new check: Object

should give you a collection with one critique (because the Object class always has many methods ;) ). Go on click on the critique item and inspect it. You will see that there is a special "description" tab. This is the power of critique objects, they can present themselves in a different way. Guess what: you can even visualize the critique if needed.'
	