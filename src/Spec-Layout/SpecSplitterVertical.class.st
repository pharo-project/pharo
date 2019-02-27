"
A SpecSplitterVertical is the description of the addition of a vertical splitter
"
Class {
	#name : #SpecSplitterVertical,
	#superclass : #SpecLayoutAdd,
	#instVars : [
		'commands'
	],
	#category : #'Spec-Layout-Actions'
}

{ #category : #'instance creation' }
SpecSplitterVertical class >> commands: commands [

	^ self new
		commands: commands;
		yourself
]

{ #category : #protocol }
SpecSplitterVertical >> arguments [
	^ {(self commands identityIndexOf: self)}
]

{ #category : #accessing }
SpecSplitterVertical >> commands [
	
	^ commands
]

{ #category : #accessing }
SpecSplitterVertical >> commands: anObject [
	
	commands := anObject
]

{ #category : #initialization }
SpecSplitterVertical >> initialize [

	super initialize.

	selector := #newVSplitterAt:.
	layoutFrame := ((0@0 corner: 0@0) asSpecLayoutFrame 
							rightOffset: 1).
	commands := nil.
]

{ #category : #testing }
SpecSplitterVertical >> isSplitter [

	^ true
]
