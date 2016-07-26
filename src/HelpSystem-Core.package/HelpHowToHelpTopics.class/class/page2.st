pages
page2
	^HelpTopic 
		title: '2. Forming a hierarchy'
		contents: 'To form a hierarchy we just have to add new subtopics on our root topic.
		
		|root sub1 sub2|
		root := HelpTopic 
			title: ''My first topic''
			contents: ''A simple topic of interest''.		
		sub1 := HelpTopic 
			title: ''My first subtopic''
			contents: ''First subsection''.		
		sub2 := HelpTopic 
			title: ''My second subtopic''
			contents: ''Second subsection''.		
		root 
			addSubtopic: sub1; 
			addSubtopic: sub2.
		HelpBrowser openOn: root
		
		'