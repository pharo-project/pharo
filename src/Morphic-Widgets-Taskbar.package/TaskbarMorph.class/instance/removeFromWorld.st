taskbar
removeFromWorld
	"Delete the receiver from its world after restoring minimized tasks.
	Collapse those that were minimized after removal.
	Turn window animation off for the duration."

	|mins animation|
	mins := self tasks select: [:t | t isMinimized].
	animation := self theme settings animationSettings useAnimation.
	animation ifTrue: [self theme settings animationSettings useAnimation: false].
	[mins do: [:t | t morph restore; resetCollapsedFrame].
	self delete.
	mins do: [:t | t morph minimize]]
		ensure: [animation ifTrue: [self theme settings animationSettings useAnimation: true]]