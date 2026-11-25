Extension { #name : 'GrowlMorph' }

{ #category : '*SystemCommands-SourceCodeCommands' }
GrowlMorph class >> showTestResult: testResult with: message [

	| color |
	color := Color gray.
	testResult hasPassed ifTrue: [
		color := TestResult defaultColorBackGroundForPassingTest ].
	testResult hasFailures ifTrue: [
		color := TestResult defaultColorBackGroundForFailureTest ].
	testResult hasErrors ifTrue: [
		color := TestResult defaultColorBackGroundForErrorTest ].

	^ self
		  openWithLabel: message
		  contents: testResult printString
		  backgroundColor: color
		  labelColor: Color black
]
