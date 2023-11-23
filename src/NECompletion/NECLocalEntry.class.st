"
I represent a local variable: either a temporary variables, a block temporary or a method arguments.
"
Class {
	#name : 'NECLocalEntry',
	#superclass : 'NECCVariableEntry',
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'accessing' }
NECLocalEntry >> label [
	^ 'local variable'
]
