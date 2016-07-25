validation
validate: anObject on: aPort 
	"This gets triggered when aPort wants to be changed.
	The presentation has a chance to veto the value."
	self shouldValidate ifFalse: [ ^ true ].
	^ self doValidate: anObject on: aPort.