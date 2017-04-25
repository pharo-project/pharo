examples - filtering
gtExampleHelpSearchOn: aSpotter
	<gtExample>
	<label: 'Help search'>
	<description: 'A spotter for searching through help pages.'>
	<dependsOn: #gtExampleBasic>
	
	^ aSpotter
		filterByMethods: {
			GTSpotter>>#spotterForHelpTopicFor:.
			HelpTopic>>#spotterForHelpTopicFor:}