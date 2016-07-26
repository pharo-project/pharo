actions api
addStep: aSpotterStep during: aBlock
	"Adds aSpotterStep without announcements,
	evaluates aBlock and removes previously added
	step after evaluation.
	May be used to determine if it is possible to dive in
	a candidate by adding a fake step to simulate dive in.
	aSpotterStep must not be nil
	aBlock must not be nil"
	self assert: aSpotterStep isNotNil.
	self assert: aBlock isNotNil.
	
	self steps add: aSpotterStep.
	aBlock ensure: [ 
		self steps remove: aSpotterStep ]