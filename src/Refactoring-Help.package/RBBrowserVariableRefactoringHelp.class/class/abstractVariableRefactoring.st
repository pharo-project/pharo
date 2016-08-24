pages
abstractVariableRefactoring
	^HelpTopic
		title: 'Abstract'
		contents: 
'
Shows a list of variables from the class or instance side, creates an accessors for the variable and replaces all direct access to this variable by this accessors method.
(For this class and all of its subclasses.)
There is no special handling for already existing accessors methods, their direct access is replaced too. And if an accessors method with the name of this variable already exists, the newly created method will get the same name with a counter suffix.
'
