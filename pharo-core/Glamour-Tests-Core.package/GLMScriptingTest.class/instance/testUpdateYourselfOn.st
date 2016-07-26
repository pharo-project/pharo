tests
testUpdateYourselfOn

	| announcer entity browser presentation |
	announcer := Announcer new.
	entity := OrderedCollection with: 1 with: 2 with: 3.
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a |
		presentation := a list 
			updateOn: GLMTestAnnouncement from: [ announcer ];
			shouldValidate: true ].
	browser startOn: entity.
	presentation registerAnnouncements.
	presentation selection: 3.
	entity removeLast.
	announcer announce: GLMTestAnnouncement.
	self assert: presentation selection isNil 