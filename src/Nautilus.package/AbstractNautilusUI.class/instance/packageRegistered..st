system announcements
packageRegistered: anAnnouncement
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	((anAnnouncement package name beginsWith: 'ConfigurationOf') or: [ anAnnouncement package name beginsWith: 'BaselineOf' ])
		ifTrue:
			[ | configurationGroup |
			configurationGroup := self groupsManager groups detect: [ :group | group name = self model class configurationsGroupName ].
			self addPackages: anAnnouncement package asOrderedCollection inGroup: configurationGroup ].
	self updatePackageView