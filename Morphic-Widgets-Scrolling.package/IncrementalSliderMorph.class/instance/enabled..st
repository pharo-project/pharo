accessing
enabled: aBoolean
	"Set whether the receiver is enabled for user input."

	self sliderMorph ifNotNil: [:sm | sm enabled: aBoolean].
	self
		changed: #enabled;
		changed: #minEnabled;
		changed: #maxEnabled