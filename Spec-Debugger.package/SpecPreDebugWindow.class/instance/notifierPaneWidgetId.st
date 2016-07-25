building widgets
notifierPaneWidgetId
	"Return the id of the notifier widget. Assume there is just one widget that is not a button,
	which represents the notifier."
	
	self buildNotifierPaneWidgetsSpec pairsDo: [ :key :value |
		(self widgets keys includes: key) ifTrue: [ ^ key ] ].
	^ nil