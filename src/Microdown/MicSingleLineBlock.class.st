"
I characterize one liners. 
They do not know consumeLine: because they are composed of only one line. 
"
Class {
	#name : #MicSingleLineBlock,
	#superclass : #MicAbstractBlock,
	#category : #'Microdown-Model'
}

{ #category : #public }
MicSingleLineBlock >> addLineAndReturnNextNode: line [
	"By default skip the rest of the line."

	^ self
]

{ #category : #testing }
MicSingleLineBlock >> canConsumeLine: line [
	"return if this block can consume line"
	
	^ false 
]
