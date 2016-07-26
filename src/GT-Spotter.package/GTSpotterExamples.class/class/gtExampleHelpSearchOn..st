examples - filtering
gtExampleHelpSearchOn: aSpotter
	<gtExample>
	<label: 'Help search'>
	<description: 'A spotter for searching through help pages.'>
	<depends: #gtExampleBasic>
	
	^ aSpotter
		filterByMethods: {
			GTSpotter>>#spotterForHelpTopicFor:.
			HelpTopic>>#spotterForHelpTopicFor:}