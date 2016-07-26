pages
step1
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