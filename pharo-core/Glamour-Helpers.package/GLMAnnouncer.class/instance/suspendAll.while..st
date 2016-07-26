suspending
suspendAll: aCollection while: aBlock
	| previousSuspendedAnnouncemets |
	previousSuspendedAnnouncemets := suspendedAnnouncemets.
	suspendedAnnouncemets := aCollection copy.
	aBlock ensure: [suspendedAnnouncemets := previousSuspendedAnnouncemets] 