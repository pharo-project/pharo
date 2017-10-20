private
openWithSpecLayout: aSpec

	window value: (self buildWithSpecLayout: aSpec).
	self changed: #open with: #().
	self updateTitle.