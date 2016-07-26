tests
testTransmissionTriggeredAnnounced
	| browser announcements pane1 pane2 transmission |
	announcements := OrderedCollection new.
	browser := GLMBrowser new.
	pane1 := GLMPane new.
	pane2 := GLMPane new.
	browser addPane: pane1.
	browser addPane: pane2.
	transmission := GLMTransmission new.
	transmission addActiveOrigin: (pane1 port: #origin).
	transmission destination: (pane2 port: #destination).
	browser addTransmission: transmission.
	browser
		when: GLMTransmissionTriggered
		do: [ :ann | announcements add: ann ].
	self assert: announcements isEmpty.
	transmission transmit.
	self assert: announcements size equals: 1