protocol
defaultOnce: aButton
	aButton privateSetState: true.
	self currentActivated: aButton.
	"This method is used to avoid a problem of default: with a DynamicComposableModel but a fix would be better (if possible)"