*Polymorph-Widgets
expandBoxHit
	"The fullscreen expand box has been hit"

	self isCollapsed
		ifTrue: [self playRestoreUpSound.
				self
					hide;
					collapseOrExpand.
				self unexpandedFrame ifNil: [self unexpandedFrame: fullFrame].
				self
					fullscreen;
					setExpandBoxBalloonText.
				^self show].
	self unexpandedFrame
		ifNil: [self playMaximizeSound.
				self theme settings animationSettings useAnimation ifTrue: [self animateMaximize].
				self
					unexpandedFrame: fullFrame;
					fullscreen]
		ifNotNil: [self playRestoreDownSound.
				self theme settings animationSettings useAnimation ifTrue: [self animateRestore].
				self
					bounds: self unexpandedFrame;
					unexpandedFrame: nil].
	self setExpandBoxBalloonText