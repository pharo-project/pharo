action
showPreview
	
	self isPreviewShown ifTrue: [ ^ self ].
	
	self addBrickBack: self previewBrick.
	self previewBrick spotterModel ifNotNil: [ :aSpotter |
		aSpotter hasSelected ifTrue: [
			self previewBrick updatePreviewWith: aSpotter currentStep selected candidate ] ].
	
	self contentsBrick hSpaceFill: (100 - self previewBrick layout hPercent).
	self changed.