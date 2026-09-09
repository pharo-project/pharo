"
I represent a local variable: either a temporary variables, a block temporary or a method arguments.
"
Class {
	#name : 'CoLocalEntry',
	#superclass : 'CoVariableEntry',
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'accessing' }
CoLocalEntry >> rawLabel [
	^ 'local variable'
]
