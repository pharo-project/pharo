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

{ #category : #accessing }
SpecSplitterVertical >> commands [
	
	^ commands
]

{ #category : #accessing }
SpecSplitterVertical >> commands: anObject [
	
	commands := anObject
]

{ #category : #protocol }
SpecSplitterVertical >> generateArguments [
	| index |

	index := self commands identityIndexOf: self.

	self arguments: { index }
]

{ #category : #initialization }
SpecSplitterVertical >> initialize [
	super initialize.

	selector := #newVSplitterAt:.
	layoutFrame := (0 @ 0 corner: 0 @ 0) asSpecLayoutFrame rightOffset: 1
]

{ #category : #testing }
SpecSplitterVertical >> isSplitter [

	^ true
]
