pages
introduction
	"This method was automatically generated. Edit it using:"
	"SUnitHelp edit: #introduction"
	^HelpTopic
		title: 'Introduction'
		contents: 
'SUnit is the mother of all unit testing frameworks, and serves as one of the cornerstones of test-driven development methodologies such as eXtreme Programming. 

The culture of testing has always been part of the philosophy of development in Smalltalk:
You write a method, accept the code and test it by writing a small script in a workspace, in a comment or an example as a class method. But this approach does not allow to automatically repeat the testing or ensure that the code is working. SUnit is a framework you can use to describe and execute unit tests. Unit testing is a method by which individual units of source code are tested to determine if they are fit for use. A unit is the smallest testable part of an application.!' readStream nextChunkText