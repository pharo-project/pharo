*GT-SpotterExtensions-Core
spotterForHelpTopicFor: aStep
	<spotterOrder: 200>
	self flag: 'filter for multiple items like #title + #contents (combined by OR)'.
	self flag: #specialFilter.
	aStep listProcessor 
		title: 'Help contents';
		items: [ self subtopics ];
		itemName: [ :helpTopic | helpTopic title ];
		itemIcon: [ :helpTopic | helpTopic gtTopicIcon ];
		itemFilterName: [ :helpTopic | helpTopic contents asString ]; " the filter should only scan the contents / maybe title too ? "
		filter: GTFilterSubstrings;
		wantsToDisplayOnEmptyQuery: true