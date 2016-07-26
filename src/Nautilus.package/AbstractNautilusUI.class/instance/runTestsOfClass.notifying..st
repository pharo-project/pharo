private
runTestsOfClass: aClass notifying: aBoolean
	| color result |
	aClass resetHistory.
	result := aClass suite run.
	result updateResultsInHistory.
	
	aBoolean ifFalse: [ ^self ].
	color := Color gray.
	result hasPassed ifTrue: [ color :=  Color green ].
	result hasFailures ifTrue: [ color :=  Color yellow ].
	result hasErrors ifTrue: [ color :=  Color red ].
			
	self 
		notifyTitle: aClass name 
		contents: result printString
		color: color