pages
step1
	^HelpTopic 
		title: 'Step 1 - Create a class for the book'
		contents:
		'STEP 1 - CREATE A CLASS FOR THE BOOK
		
There is a predefined class CustomHelp which you have to subclass for a custom help book to show up as a book in the Help browser:
	
  CustomHelp subclass: #MyAppHelp
    	instanceVariableNames: ''''
	  classVariableNames: ''''
	  poolDictionaries: ''''
	  category: ''MyApp-Help'' 

Class methods on this class can reflect pages and if you want to provide nested help books just subclass your own help class to form a hierarchy. Any new subclass of MyAppHelp will then be a new book in your hierarchy.

The class category used should end with "-Help" so it is easy to recognize that it includes the help support of your project.'
