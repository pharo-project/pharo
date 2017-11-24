"
A postscript evaluated after unloading/removing
"
Class {
	#name : #MCRemovalPostscriptDefinition,
	#superclass : #MCScriptDefinition,
	#category : #'Monticello-Modeling'
}

{ #category : #accessing }
MCRemovalPostscriptDefinition class >> scriptSelector [
	^ #postscriptOfRemoval 
]

{ #category : #installing }
MCRemovalPostscriptDefinition >> unload [
	super unload.
	self evaluate
]
