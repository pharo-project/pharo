pages
step5
	"This method was automatically generated. Edit it using:"
	"SUnitTutorial edit: #step5"
	^HelpTopic
		title: 'Step 5 - Getting the test green'
		contents: 
'To get our test green we have to implement the missing methods. First the debugger tells us that the Person class does not understand the message #named: 

We dont have to go back into a standard browser to implement it, we just select ''Create'' in the walkback window. The system now asks us where #named: should be defined in the inheritance hierarchy. We select the Person class and categorize the method in a new method category ''instance creation''.

Finally we have a debugger open where we can implement the code like this:

  named: aString
 	^(self new)
		name: aString;
		yourself
		
Note that after accepting the method in the debugger you can instantly step through it or hit ''Proceed'' to continue execution. The system tell us that there is no setter method #name:, so we implement it too on the instance side:
	
   name: aString
	name := aString

Next the #firstName: message is missing:
	
   firstName: aString
	firstName := aString
	
At any point in time we can control if the test is already green in the TestRunner. Finally when we implement
the missing #fullName method by concatenating first name and surename:

	fullName
		^firstName, '' '', name  
		
Now out test should be green.!' readStream nextChunkText