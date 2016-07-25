transformations
atMostAsLuminentAs: aFloat 
	| revisedColor |
	revisedColor := self.
	[ revisedColor luminance > aFloat ] whileTrue: [ revisedColor := revisedColor slightlyDarker ].
	^ revisedColor