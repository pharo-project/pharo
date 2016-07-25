instance creation
new
	| instance |
	instance := super new.
	instance index: (Process allocatePSKey: instance).
	^ instance