"
A preamble evaluated before unloading/removing
"
Class {
	#name : #MCRemovalPreambleDefinition,
	#superclass : #MCScriptDefinition,
	#category : #'Monticello-Modeling'
}

{ #category : #accessing }
MCRemovalPreambleDefinition class >> scriptSelector [
	^ #preambleOfRemoval
]

{ #category : #accessing }
MCRemovalPreambleDefinition >> sortKey [
	^ 'zzz' "force to the end so it gets unloaded early"
]

{ #category : #installing }
MCRemovalPreambleDefinition >> unload [
	super unload.
	self evaluate
]
