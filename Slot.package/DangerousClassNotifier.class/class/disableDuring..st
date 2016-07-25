accessing
disableDuring: aBlock
	"execute a block with me disabled. Works even nested and restores the old state"
	| oldState |
	oldState := enabled.
	enabled := false.
	aBlock value.
	enabled := oldState.