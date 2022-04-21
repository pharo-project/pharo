"
I represent a specification of a code styler.
Basically just encapsulates a class and a method specified by class side methods with the `codeblockStylerFor:` pragma.


I implement the method `style: someCode` which will return a styled Text for someCode.

On my class side I cache a dictionary to look up and reset the stylers.
I explicitly not keeping references to class or method, but only class names and selector name. 
The purpose is to make sure I do not refer to obsolete classes or methods.
"
Class {
	#name : #MicCodeStylerSpec,
	#superclass : #Object,
	#instVars : [
		'classSymbol',
		'selector'
	],
	#classInstVars : [
		'styleCache'
	],
	#category : #'Microdown-RichTextComposer-Composer'
}

{ #category : #cache }
MicCodeStylerSpec class >> buildCache [
	styleCache := Dictionary
		newFromAssociations: ((PragmaCollector
			filter: [ :prg | 
				prg selector = 'codeblockStyler:' and: [ 
					"When debugging stylers, make sure no old ones are included"
				prg method methodClass isObsolete not ] ]) reset
			collected collect: [ :prg | prg arguments first asLowercase -> (self new 
				classSymbol:  prg method methodClass soleInstance name; 
				selector: prg method selector) ])
]

{ #category : #stylers }
MicCodeStylerSpec class >> codeStylerForMethod: aString [
	<codeblockStyler: 'Method'>
	^MicSmalltalkTextStyler new 
		isForWorkspace: false; 
		styledTextFor: aString asText
]

{ #category : #stylers }
MicCodeStylerSpec class >> codeStylerForPharo: aString [
	<codeblockStyler: 'Pharo'>
	"For full methods use style 'Method'"
	^MicSmalltalkTextStyler new 
		isForWorkspace: true; 
		styledTextFor: aString asText
]

{ #category : #stylers }
MicCodeStylerSpec class >> codeStylerForSmalltalk: aString [
	<codeblockStyler: 'Smalltalk'>
	^MicSmalltalkTextStyler new 
		isForWorkspace: true; 
		styledTextFor: aString asText
]

{ #category : #stylers }
MicCodeStylerSpec class >> defaultStyler: aString [
	<codeblockStyler: 'default'>
	"Pay attention without this one the logic can loop."
	
	^ self codeStylerForPharo: aString
]

{ #category : #initialization }
MicCodeStylerSpec class >> initializeCache [
	<script>
	styleCache := nil.
]

{ #category : #accessing }
MicCodeStylerSpec class >> style: aString asLanguage: aLanguage [

	^ aString 
		ifEmpty: [ aString asText ]
		ifNotEmpty: [(self styleFor: aLanguage) style: aString]
]

{ #category : #accessing }
MicCodeStylerSpec class >> styleCache [
	styleCache ifNil: [ self buildCache ].
	^ styleCache 
]

{ #category : #accessing }
MicCodeStylerSpec class >> styleFor: aLanguage [
	^ self styleCache at: aLanguage asLowercase
		ifAbsent: [ self styleFor: 'default' ]
]

{ #category : #stylers }
MicCodeStylerSpec class >> textStyler: aString [
	<codeblockStyler: 'text'>
	
	^aString asText
]

{ #category : #accessing }
MicCodeStylerSpec >> classSymbol [

	^ classSymbol
]

{ #category : #accessing }
MicCodeStylerSpec >> classSymbol: anObject [

	classSymbol := anObject
]

{ #category : #accessing }
MicCodeStylerSpec >> selector [

	^ selector
]

{ #category : #accessing }
MicCodeStylerSpec >> selector: anObject [

	selector := anObject
]

{ #category : #styler }
MicCodeStylerSpec >> style: someCode [
	"This method uses perform:with: - it is part of the scheme to not store references to classes or compiled methods"
	^ (self class environment at: classSymbol) 
			perform: self selector 
			with: someCode 
]
