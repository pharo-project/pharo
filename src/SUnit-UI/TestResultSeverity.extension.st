Extension { #name : #TestResultSeverity }

{ #category : #'*SUnit-UI' }
TestResultSeverity >> color [
	"Returns the color to be used when the test is displayed to user."
	^ self subclassResponsibility
]
