porcine capture
openMorphicWindowForSuspendedProcess: aProcess
	| menu rules |
	menu := UIManager default newMenuIn: self for: self.
	"nickname  allow-stop  allow-debug"
	rules := self  processBrowser nameAndRulesFor: aProcess.
	menu add: 'Dismiss this menu' 
		target: menu 
		selector: #delete; 
		addLine.
	menu add: 'Open Process Browser' 
		target: self processBrowser 
		selector: #open.
	menu add: 'Resume'
		target: self
		selector: #resumeProcess:fromMenu:
		argumentList: { aProcess . menu }.
	menu add: 'Terminate'
		target: self
		selector: #terminateProcess:fromMenu:
		argumentList: { aProcess . menu }.
	rules third ifTrue: [
		menu add: 'Debug at a lower priority'
			target: self
			selector: #debugProcess:fromMenu:
			argumentList: { aProcess . menu }.
	].
	menu addTitle: aProcess identityHash asString,
		' ', rules first,
		' is taking too much time and has been suspended.
What do you want to do with it?'.
	menu stayUp: true.
	menu popUpInWorld
