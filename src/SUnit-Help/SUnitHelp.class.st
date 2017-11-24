"
This is a custom help book for the SUnit framework.

"
Class {
	#name : #SUnitHelp,
	#superclass : #CustomHelp,
	#category : #'SUnit-Help'
}

{ #category : #accessing }
SUnitHelp class >> bookName [
	^'SUnit framework'
]

{ #category : #pages }
SUnitHelp class >> introduction [
	"This method was automatically generated. Edit it using:"
	"SUnitHelp edit: #introduction"
	^HelpTopic
		title: 'Introduction'
		contents: 
'SUnit is the mother of all unit testing frameworks, and serves as one of the cornerstones of test-driven development methodologies such as eXtreme Programming. 

The culture of testing has always been part of the philosophy of development in Smalltalk:
You write a method, accept the code and test it by writing a small script in a workspace, in a comment or an example as a class method. But this approach does not allow to automatically repeat the testing or ensure that the code is working. SUnit is a framework you can use to describe and execute unit tests. Unit testing is a method by which individual units of source code are tested to determine if they are fit for use. A unit is the smallest testable part of an application.!' readStream nextChunkText
]

{ #category : #pages }
SUnitHelp class >> links [
	"This method was automatically generated. Edit it using:"
	"SUnitHelp edit: #links"
	^HelpTopic
		title: 'Links'
		contents: 
'Visit the following sites to get more informations on SUnit

- http://sunit.sourceforge.net/
- http://www.iam.unibe.ch/~ducasse/Programmez/OnTheWeb/Eng-Art8-SUnit-V1.pdf
- http://en.wikipedia.org/wiki/SUnit!' readStream nextChunkText
]

{ #category : #accessing }
SUnitHelp class >> pages [
	^#(introduction running SUnitTutorial SUnitAPIDocumentation links)
]

{ #category : #pages }
SUnitHelp class >> running [
	"This method was automatically generated. Edit it using:"
	"SUnitHelp edit: #running"
	^HelpTopic
		title: 'Running tests'
		contents: 
'SUnit allows writing tests on Smalltalk source code and checking results. This is usually done in a tool called TestRunner which you can either open from the Pharo world menu or by evaluating:

   TestRunner open

in a Smalltalk workspace.

By writing tests and checking them with the test runner you can ensure the sustainability of your applications.

!' readStream nextChunkText
]
