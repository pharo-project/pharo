tests
testUnregisterAnnouncementsWhenRemovingPane
	| announcer browser presentation actualFirstPresentation actualLastPresentation |
	announcer := GLMAnnouncer new.
	browser := GLMFinder new.
	presentation := GLMPresentation new.
	presentation title: [ :entity | entity ].
	browser addPresentation: presentation.
	presentation updateOn: GLMTestAnnouncement from: [ announcer ].
	browser entity: #value1.
	actualFirstPresentation := browser panes first presentations first.
	actualFirstPresentation registerAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 1.
	self assert: announcer glmSubscriptions anyOne announcementClass equals: GLMTestAnnouncement.
	self assert: announcer glmSubscriptions anyOne subscriber presentation equals: actualFirstPresentation.
	self assert: announcer glmSubscriptions anyOne subscriber presentation == actualFirstPresentation.
	(browser panes first port: #selection) value: #value2.
	browser panes last presentations first registerAnnouncements.
	actualLastPresentation := browser panes last presentations first.
	self assert: announcer numberOfSubscriptions equals: 2.
	self
		assert: (announcer glmSubscriptions allSatisfy: [ :each | each announcementClass = GLMTestAnnouncement ]).
	self
		assert:
			(announcer glmSubscriptions anySatisfy: [ :each | each subscriber presentation = actualFirstPresentation ]).
	self
		assert:
			(announcer glmSubscriptions
				anySatisfy: [ :each | each subscriber presentation == actualFirstPresentation ]).
	(browser panes first port: #selection) value: #value3.
	actualLastPresentation := browser panes last presentations first.	"because we removed the other pane"
	self assert: announcer numberOfSubscriptions equals: 1.
	self assert: announcer glmSubscriptions anyOne announcementClass equals: GLMTestAnnouncement.
	self assert: announcer glmSubscriptions anyOne subscriber presentation equals: actualFirstPresentation.
	self assert: announcer glmSubscriptions anyOne subscriber presentation == actualFirstPresentation