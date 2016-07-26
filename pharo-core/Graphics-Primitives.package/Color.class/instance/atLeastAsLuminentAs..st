transformations
atLeastAsLuminentAs: aFloat 
	| revisedColor |
	revisedColor := self.
	[ revisedColor luminance < aFloat ] whileTrue: [ revisedColor := revisedColor slightlyLighter ].
	^ revisedColor