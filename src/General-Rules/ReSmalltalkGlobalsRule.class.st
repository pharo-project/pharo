"
Do not send requests to ""Smalltalk"" (which models the whole image)  that are related to the envionment of defines classes and globals
"
Class {
	#name : 'ReSmalltalkGlobalsRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReSmalltalkGlobalsRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReSmalltalkGlobalsRule class >> ruleName [
	^ 'Use "Smalltalk globals" instead of "Smalltalk"'
]

{ #category : 'accessing' }
ReSmalltalkGlobalsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^ 'SmalltalkGlobalsRule'
]

{ #category : 'initialization' }
ReSmalltalkGlobalsRule >> initialize [
	super initialize.
	self
		replace: 'Smalltalk allClasses'
			with: 'Smalltalk globals allClasses';
		replace: 'Smalltalk allClassesDo: `@statements'
			with: 'Smalltalk globals allClassesDo: `@statements';
		replace: 'Smalltalk allTraits'
			with: 'Smalltalk globals allTraits';
		replace: 'Smalltalk includes:`@statements'
			with: 'Smalltalk globals includes: `@statements';
		replace: 'Smalltalk flushClassNameCache'
			with: 'Smalltalk globals flushClassNameCache';
		replace: 'Smalltalk includesKey: `@statements'
			with: 'Smalltalk globals includesKey: `@statements'
]
