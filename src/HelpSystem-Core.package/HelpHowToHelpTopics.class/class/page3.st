pages
page3
	^HelpTopic 
		title: '3. Adding icons'
		contents: 'If you dont like the default icon you can add own custom icons to the topics. See the class HelpIcons for more details.
		
		|root sub1 sub2|
		root := HelpTopic 
			title: ''My first topic''
			contents: ''A simple topic of interest''.		
		sub1 := HelpTopic 
			title: ''My first subtopic''
			contents: ''First subsection''.		
		sub2 := HelpTopic 		     
			title: ''My second subtopic''
			iconName: #packageIcon
			contents: ''Second subsection''.		
		root 
			addSubtopic: sub1; 
			addSubtopic: sub2.
		HelpBrowser openOn: root
		
		'