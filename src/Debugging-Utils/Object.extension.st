Extension { #name : 'Object' }

{ #category : '*Debugging-Utils' }
Object >> halt [
	"This is the typical message to use for inserting breakpoints during debugging."

	<debuggerCompleteToSender>
	Halt now
]

{ #category : '*Debugging-Utils' }
Object >> halt: aString [
	<debuggerCompleteToSender>
	Halt now: aString
]

{ #category : '*Debugging-Utils' }
Object >> haltFromCount: anInteger [
	"Always halt after a count has been reached"

	<debuggerCompleteToSender>
	Halt fromCount: anInteger
]

{ #category : '*Debugging-Utils' }
Object >> haltIf: condition [
	<debuggerCompleteToSender>

	Halt if: condition
]

{ #category : '*Debugging-Utils' }
Object >> haltIfNil [
]

{ #category : '*Debugging-Utils' }
Object >> haltIfNotTest [
	"Halt if execution was not started by a test. This is useful as this means one can just run tests undisturbed, while a execution for example from the Playground would be halted"
	<debuggerCompleteToSender>
	Halt ifNotTest
]

{ #category : '*Debugging-Utils' }
Object >> haltIfTest [
	"Halt if execution was started by a test. These #halts are just active in the tests, useful when you want to add a halt in methods that are executed a lot (e.g. collections)"
	<debuggerCompleteToSender>
	Halt ifTest
]

{ #category : '*Debugging-Utils' }
Object >> haltOnCount: anInteger [
	"Halts once when a count is reached"

	<debuggerCompleteToSender>
	Halt onCount: anInteger
]

{ #category : '*Debugging-Utils' }
Object >> haltOnce [
	<debuggerCompleteToSender>
	Halt once
]
