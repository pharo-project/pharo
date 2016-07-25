pages
step2
	^HelpTopic
		title: 'Step 2 - Provide a book name'
		contents:
	'STEP 2 - PROVIDE A BOOK NAME
	
Now implement the class method #bookName to return the name of your help book.

   bookName
	    ^''My App help''
	
By implementing this method the system knows how you would like to name your book and uses the given string as a label in the HelpBrowser later.

Additionaly you can specify #bookDescription which will be displayed in the HelpBrowser when one selects your book. The description han be added in the following way:

	bookDescription
	    ^ ''This is a help book about my App'''
