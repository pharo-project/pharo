pages
page1
	^HelpTopic 
		title: '1. Simple help topics'
		contents: 'The help browser usually operates on a hierarchy of help topics with one help topic at the root level. Evaluate the following expression in a workspace to contruct a simple help topic and open it as a root topic in the help browser.
		
		|root|
		root := HelpTopic 
			title: ''My first topic''
			contents: ''A simple topic of interest''.					
		
		HelpBrowser openOn: root
		
		
Note that the help browser displays the contents of our topic in the right page and uses the topics title as the title for the help browser window.	
		'