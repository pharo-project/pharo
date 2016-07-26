updating
spawnFilterUpdateThread
	"Runs in background, thank to Henrik Johansen for this"

	[ | oldPattern |
	oldPattern := nil.
	[ isEditingSemaphore wait.
	"If pattern has changed, see if we need to filter.
	If not, it's probably an extraneous signal received while we were waiting for 0.2 seconds, and we discard then till we end up waiting for filterChangeSemaphore again"
	oldPattern ~= pattern
		ifTrue: [ oldPattern := pattern.
			0.2 seconds wait.
			"Pattern still the same? If not, just loop again and end up waiting for another 0.2 secs"
			oldPattern = pattern
				ifTrue: [ self filter ] ] ] repeat ] forkAt: Processor userBackgroundPriority