singleton
resetDefault

	"self resetDefault"
	default  ifNotNil: [
		"When an Organizer is not used anymore, we should always pay attention that these two actions are corretly done."
		default unregisterInterestToSystemAnnouncement.
		
		self flag: #hack. "for decoupling MC"
		self class environment at: #MCWorkingCopy ifPresent: [ :wc |
			wc removeDependent: default]].
	default := self new.
	self packageClass initialize