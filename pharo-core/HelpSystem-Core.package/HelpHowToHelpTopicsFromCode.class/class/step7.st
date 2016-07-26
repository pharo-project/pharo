pages
step7
	^HelpTopic
		title: 'Step 7 - Tips and Tricks'
		contents:
	'STEP 7 - TIPS AND TRICKS

Tip1:
          If you implement the #pages method you can also 
          use the name of a custom help class that should be 
          integrated between the specific pages:
 
              #pages
                   ^(firstPage MyAppTutorial secondPage)

Tip2:
          You can easily edit the help contents of a page by 
          using the #edit: message. For our example just evaluate:

			MyAppHelp edit: #firstPage
			
	     This will open a workspace with the help contents and 
	     when you accept it it will be saved back to the help 
	     method defining the topic.		  
		

'
