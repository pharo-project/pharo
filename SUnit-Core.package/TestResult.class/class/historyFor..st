history
historyFor: aTestCaseClass
	"I return the last test dictionary for aTestCaseClass. 
	If none found, I return an empty dictionary but will not link it to the class in the history."
	
	| history |
	history := aTestCaseClass history.
	history ifNil: [ ^ self newTestDictionary ].
	^ history
	
"	^ self history at: aTestCaseClass ifAbsent: [ self newTestDictionary ]"