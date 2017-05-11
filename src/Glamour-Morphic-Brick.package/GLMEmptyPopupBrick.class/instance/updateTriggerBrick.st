private
updateTriggerBrick
	| aTriggerImage |
	
	aTriggerImage := self anchorBrick brickImageForm: self themer popupThemer backgroundColor.
	self triggerBrick icon: aTriggerImage