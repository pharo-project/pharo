"
I'm like TextDiffBuilder, but I use the pretty-printed version of the source code if available.

Instance Variables
	sourceClass:		<Class>

sourceClass
	- this class provides the pretty-printer

"
Class {
	#name : #PrettyTextDiffBuilder,
	#superclass : #TextDiffBuilder,
	#instVars : [
		'sourceClass'
	],
	#category : #Text-Diff
}

{ #category : #'instance creation' }
PrettyTextDiffBuilder class >> from: srcString to: dstString inClass: srcClass [ 
	^ (self new sourceClass: srcClass) from: srcString to: dstString
		
]

{ #category : #initialize }
PrettyTextDiffBuilder >> sourceClass: aClass [
	sourceClass := aClass.
]

{ #category : #initialize }
PrettyTextDiffBuilder >> split: aString [ 
	| formatted trimmed |
	trimmed := aString asString trimBoth.
	trimmed isEmpty ifTrue: [ ^super split: '' ].
	formatted := [ sourceClass
				source: trimmed;
				class: sourceClass;
				format] on: Error do: [ :ex | trimmed ].
	^ super split: formatted
]
