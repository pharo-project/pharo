updating
requestRefresh
	"I send the presentation a request to update that the presentation can ignore depending
	on it's state. For example the presentation can ingnore this if it is not visible on screen.
	Use #update to force a refresh."
	self pane ports do: [:each |
		(self validate: each value on: each) ifFalse: [each resetValue] ].
	self announce: (GLMPresentationRefreshRequest new presentation: self)