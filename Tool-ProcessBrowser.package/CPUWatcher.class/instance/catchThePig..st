porcine capture
catchThePig: aProcess
	| rules  |
	"nickname, allow-stop, allow-debug"
	rules := self processBrowser nameAndRulesFor: aProcess.

	(self processBrowser isUIProcess: aProcess)
		ifTrue: [ "aProcess debugWithTitle: 'Interrupted from the CPUWatcher'." ]
		ifFalse: [ 
			rules second ifFalse: [ ^self ].
			self processBrowser suspendProcess: aProcess.
			self openWindowForSuspendedProcess: aProcess ]
