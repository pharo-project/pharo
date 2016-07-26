protocol
newStateForEnabled: aBoolean
	"aBoolean here must be false"
	self deny: aBoolean.
	
	^ TabDisabled tab: self tab