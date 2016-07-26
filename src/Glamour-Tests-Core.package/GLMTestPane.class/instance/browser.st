accessing
browser
	browser isNil ifTrue: [ ^ GLMNoBrowser new ].
	^ browser