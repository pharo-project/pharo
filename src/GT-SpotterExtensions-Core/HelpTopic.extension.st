Extension { #name : #HelpTopic }

{ #category : #'*GT-SpotterExtensions-Core' }
HelpTopic >> gtAllSubtopics [
	 ^ self subtopics flatCollect: [:aTopic |
		aTopic asOrderedCollection, aTopic gtAllSubtopics ]
		 
]

{ #category : #'*GT-SpotterExtensions-Core' }
HelpTopic >> gtBreadcrumbTitle [
	|breadcrumbTitle currentHelpTopic|
	
	currentHelpTopic := self.	
	breadcrumbTitle:= currentHelpTopic title.
	[ currentHelpTopic owner notNil and: [currentHelpTopic owner owner notNil ] ] whileTrue: [ 
		currentHelpTopic := currentHelpTopic owner.
		breadcrumbTitle := breadcrumbTitle , ' > ', currentHelpTopic title  ].
	^ breadcrumbTitle 
]

{ #category : #'*GT-SpotterExtensions-Core' }
HelpTopic >> gtTopicIcon [
	^ self hasSubtopics
		ifTrue: [ Smalltalk ui icons iconNamed: #bookIcon ]
		ifFalse: [ Smalltalk ui icons iconNamed: #pageIcon ]
]

{ #category : #'*GT-SpotterExtensions-Core' }
HelpTopic >> spotterActDefault [
	HelpBrowser openOn: self
]

{ #category : #'*GT-SpotterExtensions-Core' }
HelpTopic >> spotterForHelpTopicFor: aStep [
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
]

{ #category : #'*GT-SpotterExtensions-Core' }
HelpTopic >> spotterPreviewContentHelpIn: aComposite [
	<spotterPreview: 10>
	
	self hasSubtopics ifTrue: [ ^ self ].
	aComposite text
		title: [ self title ];
		display: [:aHelpTopic | 
			aHelpTopic contents];
		entity: self.
		
	self flag: 'move "entity: self" somewhere else, maybe'
]

{ #category : #'*GT-SpotterExtensions-Core' }
HelpTopic >> spotterPreviewSubtopicsHelpIn: aComposite [
	<spotterPreview: 15>

	aComposite list
		title: [ self title ];
		display: #yourself;
		format: [ :aHelpTopic | GTSpotterItemBrick new 
				text: aHelpTopic title;
				icon: aHelpTopic gtTopicIcon;
				disable ];
		styled: [ :brick | brick ];
		entity: self subtopics.
		
	self flag: 'move "entity: self" somewhere else, maybe'
]
