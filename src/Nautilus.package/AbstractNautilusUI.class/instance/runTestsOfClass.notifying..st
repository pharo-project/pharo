private
runTestsOfClass: aClass notifying: aBoolean
	| color result |
	aClass resetHistory.
	result := aClass suite run.
	result updateResultsInHistory.
	
	aBoolean ifFalse: [ ^self ].
	color := Color gray.
	result hasPassed ifTrue: [ color := self theme successBackgroundColor ].
	result hasFailures ifTrue: [ color := self theme warningBackgroundColor ].
	result hasErrors ifTrue: [ color := self theme dangerBackgroundColor ].
			
	self 
		notifyTitle: aClass name 
		contents: result printString
		color: color