"
A SpecSplitterVertical is the description of the addition of a vertical splitter
"
Class {
	#name : #SpSplitterVertical,
	#superclass : #SpLayoutAdd,
	#instVars : [
		'commands'
	],
	#category : #'Spec2-Layout-Deprecated'
}

{ #category : #'instance creation' }
SpSplitterVertical class >> commands: commands [

	^ self new
		commands: commands;
		yourself
]

{ #category : #protocol }
SpSplitterVertical >> arguments [
	^ {(self commands identityIndexOf: self)}
]

{ #category : #accessing }
SpSplitterVertical >> commands [
	
	^ commands
]

{ #category : #accessing }
SpSplitterVertical >> commands: anObject [
	
	commands := anObject
]

{ #category : #initialization }
SpSplitterVertical >> initialize [
	super initialize.

	selector := #newVSplitterAt:.
	layoutFrame := (0 @ 0 corner: 0 @ 0) asSpLayoutFrame rightOffset: 1
]

{ #category : #testing }
SpSplitterVertical >> isSplitter [

	^ true
]
