lessons
lesson01
	^Lesson
		title: 'Lesson 1'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version01:.

"Version 0.1 represents the simplest version specification possible. 

In the method #version01, version 0.1 is defined with a single package ''Example-Core-anon.8'' 
that is loaded from the repository ''http://www.example.com/Example''.

Looking at this method you will notice a couple of things. 

Immediately after the method selector you see the pragma definition:

  <version: ''0.1''>

The pragma indicates that the version created in this method should be associated with version 
''0.1'' of the Tutorial project.

Looking a little closer you see that the argument to the method, <spec>, is the only variable in
the method and it is used as the receiver to four different messages:

  - #for:do:
  - #package:with:
  - #file: 
  - #repository:

With the evaluation of each block expression, a new object is pushed on a stack and the messages
within the block are sent to the object on the top of the stack.

So the method should be read as:

  Create version ''0.1''. The #common code for version ''0.1'' (#for:do:) consists of a package named 
  ''Example-Core'' (#package:with:) whose file attribute is ''Example-Core-anon.8'' (#file:) and whose 
  repository attribute is ''http://www.example.com/Example'' (#repository:).

We can see the spec created for version 0.1 by printing the following expression:"

  (MetacelloTutorialConfig project version: ''0.1'') spec.

"Note that in creating version ''0.1'' the #common attribute is extracted out.

In addition to #common, there are pre-defined attributes for each of the platforms upon which
Metacello runs (#pharo, #squeak, #gemstone and #squeakCommon). 

#squeakCommon is used for both #pharo and #squeak."

ProfStef next.
'