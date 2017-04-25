pages
step3
	^HelpTopic
		title: 'Step 3 - Implement pages using methods'
		contents:
	'STEP 3 - IMPLEMENT PAGES USING METHODS 

Implement a page by defining a method that returns an instance of HelpPage defining a page title and a help text displayed in the help browser. 

     firstPage
     	    ^HelpTopic
	           title: ''First Page'' 
                 contents: ''Hello world''

Define a new method for each page of your book. Please group the pages in a method category called "pages".
You can also define an icon for the specific page:

     secondPage
     	    ^HelpTopic
	           title: ''Second Page'' 
    	           iconName: #packageIcon
                 contents: ''More to come''


Note: 
=====
Later we may add support for better help contents than just plain text (markup descriptions, active morphs, ...)  '
