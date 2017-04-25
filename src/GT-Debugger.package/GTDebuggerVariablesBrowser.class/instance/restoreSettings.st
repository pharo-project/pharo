actions
restoreSettings
	| currentPane presentationTitle lastActivePresentation |
	
	presentationTitle := self debugger cache at: #activePresentationTitle ifAbsent: [ ^ self ].
	currentPane := self presentations first paneNamed: #main.
	lastActivePresentation := currentPane presentations  presentations
		detect: [ :each | each title = presentationTitle ] 
		ifNone: [ ^ self ].
	currentPane lastActivePresentation: lastActivePresentation