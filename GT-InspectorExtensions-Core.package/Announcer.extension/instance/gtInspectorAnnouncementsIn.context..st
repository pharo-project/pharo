*GT-InspectorExtensions-Core
gtInspectorAnnouncementsIn: composite context: aGTContext
	<gtInspectorPresentationOrder: 40>
	| recordedAnnouncements messageSend |
	
	recordedAnnouncements := CollectionValueHolder value: OrderedCollection new.
	messageSend := MessageSend receiver: recordedAnnouncements selector: #addAnnouncementFirst:.
	self
		when: Announcement
		send: #cull:
		to: messageSend.
	aGTContext when: GLMBrowserClosing do: [ self unsubscribe: messageSend ].
	composite table
		title: 'Announcements';
		display: [ recordedAnnouncements value ];
		column: 'Time' evaluated: #key;
		column: 'Announcement' evaluated: #value;
		send: #value;
		showOnly: 50;
		format: #gtDisplayString;
		filterOn: [:text :each | Smalltalk compiler evaluate: '| date each | date := self key. each := self value. ', text for: each logged: false ];
		updateOn: Announcement from: recordedAnnouncements gtAnnouncer;
		act: [ :table | 
			recordedAnnouncements removeAll.
			table update ] 
			icon: GLMUIThemeExtraIcons glamorousCancel 
			entitled: 'Reset'