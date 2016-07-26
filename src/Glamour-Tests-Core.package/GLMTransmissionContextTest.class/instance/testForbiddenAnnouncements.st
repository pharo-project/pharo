tests
testForbiddenAnnouncements
	| context pane |
	context := GLMTransmissionContext new.
	pane := GLMPane new.
	self assert: (context canAnnounceMatchingPresentationsChanged: pane).
	context forbidToAnnounceMatchingPresentationsChanged: pane.
	self assert: (context canAnnounceMatchingPresentationsChanged: pane) not