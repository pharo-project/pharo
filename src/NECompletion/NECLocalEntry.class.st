"
I represent a local variable: either a temporary variables, a block temporary or a method arguments.
"
Class {
	#name : #NECLocalEntry,
	#superclass : #NECCVariableEntry,
	#category : #'NECompletion-Model'
}

{ #category : #accessing }
NECLocalEntry >> label [
	^ 'local variable'
]
