"
I am a query filter which specifically created for the query browser which can represent different kind of items in one list.
I delegate actual testing logic to the actual object of given browser items:

	actualObject matchesQueryBrowserFilter: self

This method is implemented by methods, classes and class comments. So this objects can be shown and filtered in query browser.
Implementors call be back with #matchesString: messge where I check given string for my pattern.

I also provide special trick to filter items by script. To activate it the pattern string should be in form of one arg block. For example:
	[ :each | each linesOfCode > 10 ] 
Such block will be evaluated with actual object of browser item.
  
Internal Representation and Key Implementation Points.

    Instance Variables
	badScript:		<Boolean>
	scriptBlock:		<BlockClosure>
"
Class {
	#name : #ClyQueryBrowserFilter,
	#superclass : #ClyItemStringFilter,
	#instVars : [
		'scriptBlock',
		'badScript'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #private }
ClyQueryBrowserFilter >> createBlockFromPattern [

	(pattern value endsWith: ']') ifFalse: [
		badScript := true.
		^self].

	[scriptBlock := self class compiler evaluate: pattern value] 
		on: Exception do: [:err | badScript := true ]
]

{ #category : #initialization }
ClyQueryBrowserFilter >> initialize [
	super initialize.
	
	badScript := false.
]

{ #category : #testing }
ClyQueryBrowserFilter >> matches: aBrowserItem [
	| actualObject |
	actualObject := aBrowserItem class = ClyBrowserItem 
		ifTrue: [ aBrowserItem actualObject] ifFalse: [ aBrowserItem ].
		
	(pattern value beginsWith: '[') ifTrue: [ 
			^self matchesByScript: actualObject].

	^actualObject matchesQueryBrowserFilter: self
]

{ #category : #testing }
ClyQueryBrowserFilter >> matchesByScript: anObject [

	badScript ifTrue: [ ^true ].
	scriptBlock ifNil: [ self createBlockFromPattern].
	badScript ifTrue: [ ^true ].	
	
	^[(scriptBlock value: anObject) ~~ false] ifError: [ false ]
]

{ #category : #testing }
ClyQueryBrowserFilter >> matchesString: aString [

	^pattern matches: aString
]
