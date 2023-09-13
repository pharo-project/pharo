"
A preamble evaluated before load
"
Class {
	#name : #MCPreambleDefinition,
	#superclass : #MCScriptDefinition,
	#category : #'Monticello-Modeling',
	#package : #Monticello,
	#tag : #Modeling
}

{ #category : #accessing }
MCPreambleDefinition class >> scriptSelector [
	^ #preamble
]

{ #category : #installing }
MCPreambleDefinition >> load [
	super load.
	self evaluate
]
