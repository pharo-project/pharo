protocol
pauseDuring: aBlock
	| previousPausedValue |
	"Don't accept any new entries while executing aBlock"
	previousPausedValue := paused.
	paused := true.
	aBlock ensure: [ paused := previousPausedValue ]