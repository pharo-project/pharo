examples - filtering
gtExampleHelpSearch
	<gtExample>
	<label: 'Help search'>
	<description: 'A spotter for searching through help pages.'>
	| spotter |
	spotter := self gtExampleBasic.	
	^ spotter
		filterByMethods: {
			GTSpotter>>#spotterForHelpTopicFor:.
			HelpTopic>>#spotterForHelpTopicFor:}