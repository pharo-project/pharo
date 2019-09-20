"
A SpecSplitterHorizontal is a description of the addition of a horizontal splitter
"
Class {
	#name : #SpSplitterHorizontal,
	#superclass : #SpLayoutAdd,
	#instVars : [
		'commands'
	],
	#category : #'Spec2-Layout-Deprecated'
}

{ #category : #'instance creation' }
SpSplitterHorizontal class >> commands: commands [

	^ self new
		commands: commands;
		yourself
]

{ #category : #protocol }
SpSplitterHorizontal >> arguments [
	^ {(self commands identityIndexOf: self)}
]

{ #category : #accessing }
SpSplitterHorizontal >> commands [
	
	^ commands
]

{ #category : #accessing }
SpSplitterHorizontal >> commands: anObject [
	
	commands := anObject
]

{ #category : #initialization }
SpSplitterHorizontal >> initialize [
	super initialize.

	selector := #newHSplitterAt:.
	layoutFrame := (0 @ 0 corner: 0 @ 0) asSpLayoutFrame bottomOffset: 1
]

{ #category : #testing }
SpSplitterHorizontal >> isSplitter [

	^ true
]
