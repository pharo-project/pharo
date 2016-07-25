initialization
initialize

	super initialize.
	state := #off.
	target := nil.
	actionSelector := #flash.
	arguments := EmptyArray.
	actWhen := #buttonUp.

	"self on: #mouseStillDown send: #dragIfAuthoring: to: self."
		"real move should include a call on dragIfAuthoring: "