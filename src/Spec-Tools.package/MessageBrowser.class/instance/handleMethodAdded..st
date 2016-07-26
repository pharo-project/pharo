announcements
handleMethodAdded: anAnnouncement
	| item sel text boolean |

	self isDisplayed ifFalse: [ ^ self ].
	refreshingBlockHolder ifNil: [ ^ self ].
	
	item := anAnnouncement method.
	
	(self shouldRefreshItem: item fromAnnouncement: anAnnouncement) ifFalse: [ ^ self ].
	((item methodClass notNil) and:[item methodClass isObsolete not]) ifFalse: [ ^ self ].
	
	boolean := textModel hasUnacceptedEdits.
	boolean ifTrue: [ text := textModel pendingText ].
	
	sel := listModel selectedItem.
	self messages: (listModel listItems add: item asFullRingDefinition; yourself).
	listModel setSelectedItem: sel.
	
	boolean ifTrue: [ textModel pendingText: text ]