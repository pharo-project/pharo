"
This is a custom help book providing a tutorial for the SUnit framework.

"
Class {
	#name : #SUnitTutorial,
	#superclass : #SUnitHelp,
	#category : #'SUnit-Help'
}

{ #category : #accessing }
SUnitTutorial class >> bookName [
	^'Tutorial'
]

{ #category : #accessing }
SUnitTutorial class >> pages [
	^#(step1 step2 step3 step4 step5)
]

{ #category : #pages }
SUnitTutorial class >> step1 [
	"This method was automatically generated. Edit it using:"
	"SUnitTutorial edit: #step1"
	^HelpTopic
		title: 'Step 1 - Define a test class'
		contents: 
'To define a series of tests in SUnit one typically creates a subclass of class TestCase. The idea is that the methods of this class will implement several tests, the instance variables represent the objects and / or the context in which these tests will be performed. 

In our example we want to write a test on a simple Person class (which is not yet in our image). If the unit under test is a single class one convention is to name the test like the class but followed with the postfix "Test". 

So to create a test for the Person class we subclass TestCase with a custom "PersonTest" class.

	TestCase subclass: #PersonTest
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''MyApp-Tests-Model''

Note that we havent created the class Person yet - so one idea of ExtremeProgramming (XP) is to write the test first as a way to describe a use case that your application has to cover. After that you write the code and the test will show you if the scenario is fullfilled. You dont have to follow this programming style and can also write tests to cover existing code.
	!' readStream nextChunkText
]

{ #category : #pages }
SUnitTutorial class >> step2 [
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
]

{ #category : #pages }
SUnitTutorial class >> step3 [
	"This method was automatically generated. Edit it using:"
	"SUnitTutorial edit: #step3"
	^HelpTopic
		title: 'Step 3 - Running our first test method'
		contents: 
'To run our test case method we can evaluate the following code snippet in a workspace:

	PersonTest run: #testInstanceCreation

If you print the result then the following will appear:

      1 run, 0 passes, 0 skipped, 0 expected failures, 0 failures, 1 errors, 0 unexpected passes

This tells us that one test has been run and one error occured while testing. Currently we dont have all messages implemented - so the test has to fail. 

If you want to debug the test to see whats happened you can evaluate:

     PersonTest debug: #testInstanceCreation



 
	!' readStream nextChunkText
]

{ #category : #pages }
SUnitTutorial class >> step4 [
	"This method was automatically generated. Edit it using:"
	"SUnitTutorial edit: #step4"
	^HelpTopic
		title: 'Step 4 - Using the TestRunner'
		contents: 
'Testing by evaluating an expression as we is not very convenient - so it is better to open the TestRunner - a user interface tool displaying all the tests in the system. You can start it either using the world menu or by evaluating 

    TestRunner open

in a workspace.

Scroll down until you find the class category ''MyApp-Tests-Model'' and select the test by clicking on the class PersonTest. It is possible to select more than one test/category. Now click on "Run selected". 

If you run the test in the test runner it will be marked red since it contains errors. The test runner will also show you the result of the test run:

     1 run, 0 passes, 0 skipped, 0 expected failures, 0 failures, 1 errors, 0 unexpected passes

If you click on the failed method in the lower right pane the debugger will open.!' readStream nextChunkText
]

{ #category : #pages }
SUnitTutorial class >> step5 [
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
]
