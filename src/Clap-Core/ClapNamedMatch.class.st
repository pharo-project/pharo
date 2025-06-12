"
I am a match for a named composite parameter; I cover at least one word (corresponding to one of my specification's aliases).
"
Class {
	#name : 'ClapNamedMatch',
	#superclass : 'ClapCompositeMatch',
	#instVars : [
		'keyword'
	],
	#category : 'Clap-Core-Activation',
	#package : 'Clap-Core',
	#tag : 'Activation'
}

{ #category : 'accessing' }
ClapNamedMatch >> child [
	"Warning: onyl use this method if you are sure there is at least one child.
	This method is used to get a positional value"

	^ children first
]

{ #category : 'matching' }
ClapNamedMatch >> completeMatchOn: aStream [
	keyword := aStream next.
	startIndex := aStream position.
	self matchChildrenOn: aStream
]

{ #category : 'testing' }
ClapNamedMatch >> isFullMatch [

	^ (self specification isFlag and: [ self specification hasPositional ])
		ifTrue: [ children size = self specification positionals size ]
		ifFalse: [ true ]
]

{ #category : 'testing' }
ClapNamedMatch >> isValid: aValidation [

	^ aValidation isValidNamedMatch: self
]

{ #category : 'matching' }
ClapNamedMatch >> word [
	^ keyword
]
