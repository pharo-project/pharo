accessing
triggerBrick
	"Return an interactable trigger brick.
	triggerBrick can not be nil"
	<return: #GLMInteractableBrick>
	
	self
		assert: [ triggerBrick isNotNil ]
		description: [ 'Trigger brick can not be nil' ].
	
	^ triggerBrick