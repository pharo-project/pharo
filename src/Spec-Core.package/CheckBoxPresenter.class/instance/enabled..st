api
enabled: aBoolean
	"<api: #boolean getter: #enabled registration: #whenEnabledChanged:>"
	"Set if the checkbox is enabled (clickable)"

	super enabled: aBoolean.
	self changed: #enabled.
