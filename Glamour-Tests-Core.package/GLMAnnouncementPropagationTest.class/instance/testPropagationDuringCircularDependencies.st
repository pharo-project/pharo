tests
testPropagationDuringCircularDependencies
	| browser announcements |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser transmit
		to: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x ] ].
	browser transmit
		from: #one;
		to: #two;
		andShow: [ :a | a list display: [ :x | 1 to: x ] ].
	browser transmit
		from: #two port: #toSelect;
		toOutsidePort: #toSelect.
	browser transmit
		fromOutsidePort: #toSelect;
		to: #one port: #selection.
	browser transmit
		fromOutsidePort: #toSelect;
		to: #two port: #selection.
	browser startOn: 100.
	((browser paneNamed: #one) port: #selection) value: 50.
	((browser paneNamed: #two) port: #toSelect) value: 42.
	self assert: ((browser paneNamed: #one) port: #selection) value equals: 42.
	self assert: ((browser paneNamed: #two) port: #selection) value equals: 42.
	announcements := GLMLogger instance announcements reverse reject: [ :each | each isKindOf: GLMTransmissionTriggered ].
	self assert: (announcements first isKindOf: GLMMatchingPresentationsChanged).
	self assert: (announcements second isKindOf: GLMPresentationsChanged).
	self assert: (announcements third isKindOf: GLMContextChanged).
	self assert: announcements third property equals: #toSelect.
	self assert: (announcements fourth isKindOf: GLMContextChanged).
	self assert: announcements fourth property equals: #toSelect.
	self assert: (announcements fourth isKindOf: GLMContextChanged).
	self assert: announcements fifth property equals: #selection.
	self assert: announcements fifth value equals: 42.
	self assert: (announcements fifth presentation isKindOf: GLMListPresentation).
	self assert: announcements fifth presentation pane name equals: #two.
	self assert: announcements sixth property equals: #selection.
	self assert: announcements sixth value equals: 42.
	self assert: (announcements sixth presentation isKindOf: GLMCompositePresentation).
	self assert: announcements sixth presentation pane name equals: #two