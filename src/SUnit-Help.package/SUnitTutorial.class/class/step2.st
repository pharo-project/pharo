pages
step2
	"This method was automatically generated. Edit it using:"
	"SUnitTutorial edit: #step2"
	^HelpTopic
		title: 'Step 2 - Define a first test method'
		contents: 
'To define a test we create a new instance side method for our first test scenario. By convention any method (in a subclass of TestCase) whose selector starts with ''test'' is supposed to be a test and can be run using the TestRunner tool:

	testInstanceCreation
		|person|
		person := Person named: ''Beck''.
		person firstName: ''Kent''.
		self assert: person fullName = ''Kent Beck''.
		
A good style is to put this method into the method category ''testing''.

When you accept the code the system may ask you about the new class Person. It''s not yet defined - but you can easily create it using the following class definition:

	Object subclass: #Person
		instanceVariableNames: ''name firstName'' 
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''MyApp-Core-Model''!' readStream nextChunkText