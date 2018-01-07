"
RG2ElementDefinition is the abstract class for representing elements of a class-alike definition (i.e., methods, variables, comment).

parent holds the RG2ClassDefinition or RG2MetaclassDefinition defining this element.
	
	
Now a RingEntityDefinition offers two APIs: one that is generic and works for all the source code entities and this is the one we just 
presented: parent, parentName and realParent. Having such interface is important to build generic tools that could manipulate 
any entities in a polymorphic way (yes no isKindOf: everywhere).

In addition, a ring method definition offers a specific interface that should only be used when you know that you are solely manipulate
specific entity such as class element: method definition, class comment, and variables. 

Here is the equivalence table

	realParent 				realClass
	parent					ringClass
	parentName			className
	
For example for a methodDefinition we will have the following:

GENERIC API
------------------
* To access the ring class definition name, use parentName
	aRG2MethodDefinition parentName
	
Example:
	(Point>>#dist:) asRing2Definition parentName
		->  #Point
		
* If you have a complete model where classes and methods are ring definition, to access the ring class definition , use parent
	aRG2MethodDefinition parent
	
Example:
	aRG2MethodDefinition(Point>>#dist:) parent
		->  aRG2ClassDefinition(Point)
		
* If you want to access the smalltalk class that contains the compiledMethod that is represented by a ringMethodDefinition, use realParent
	aRG2MethodDefinition realParent
	
Example:
	(Point>>#dist:) asRing2Definition realParent
		->  Point
		


CLASS Element specific API
------------------------------------------
* The message class returns the class of the object :). Yes as you see we could not use class and className because class is already used to refer to the class of the object.

Example:
	(Point>>#dist:) asRing2Definition class
		->  RingMethodDefinition
		
* The message className returns the name of the ring class defining the reingMethodDefinition.

Example:
	(Point>>#dist:) asRing2Definition className
		->  #Point		
		
* If you have a complete model where classes and methods are ring definition, to access the ring class definition , use parent
	aRG2MethodDefinition ringClass
	
Example:
	aRG2MethodDefinition(Point>>#dist:) ringClass
		->  aRG2ClassDefinition(Point)
		
		
* If you want to access the smalltalk class that contains the compiledMethod that is represented by a ringMethodDefinition, use realClass
	aRG2MethodDefinition realClass
	
Example:
	(Point>>#dist:) asRing2Definition realClass
		->  Point


"
Class {
	#name : #RG2ElementDefinition,
	#superclass : #RG2Definition,
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'parsing stamp' }
RG2ElementDefinition class >> basicParseAuthorAliasFrom: aString [
	"Parse an alias/name of the author from a string that is extracted from a source file. If there is no alias/name we return emtpy string."

	| tokens dateStartIndex unknown |
	"The following timestamp strings are supported (source: squeak sources archeological survey):
		<authorname><date><time>. 
		<authorname><date>
		<date><time>
		<date><time><authorname>
		<date><authorname>
		<historical>
	All fields can be separated by spaces or line ends but a separator between author alias/name and date can be missing as well"
	"unknown:= 'unknown'."
	unknown := nil.
	aString isEmptyOrNil
		ifTrue: [ ^ unknown ].
	dateStartIndex := (aString indexOf: $/) - 1.	"If there is no / character in the timestamp, no author alias/name exists"
	dateStartIndex = -1
		ifTrue: [ ^ unknown ].
	^ [ 
	"Go the start of the date string (there can be 1 or 2 digits and a space separator can be missing at the front!!)"
	(dateStartIndex >= 2 and: [ (aString at: dateStartIndex - 1) isDigit ])
		ifTrue: [ dateStartIndex := dateStartIndex - 1 ].	"Extract only those tokens that do not possible represent date or time - meaning that authorname may be at the end"
	tokens := (aString copyFrom: dateStartIndex to: aString size) substrings
		reject: [ :token | (token occurrencesOf: $/) = 2 or: [ (token occurrencesOf: $:) = 1 ] ].	"only one token should be left if author name/alias exists"
	^ tokens isEmpty
		ifTrue: [ 
			"if dateStartIndex is not 1 then the authorname may be at the beginning"
			dateStartIndex > 1
				ifTrue: [ (aString copyFrom: 1 to: dateStartIndex - 1) trimBoth ]
				ifFalse: [ unknown ] ]
		ifFalse: [ tokens first ] ]
		on: Exception
		do: [ :e | unknown ]
]

{ #category : #'parsing stamp' }
RG2ElementDefinition class >> parseAuthorAliasFrom: aString [
	"Allows other applications  to treat a different empty alias by overriding this method"
	
	^self basicParseAuthorAliasFrom: aString
]

{ #category : #'parsing stamp' }
RG2ElementDefinition class >> parseTimestampFrom: aString [
	
	^self parseTimestampFrom: aString default: nil
]

{ #category : #'parsing stamp' }
RG2ElementDefinition class >> parseTimestampFrom: aString default: anObject [
	"Parse a date-time from a timestamp-string that is extracted from a source file. If there is no timestamp, or we cannot make sense of it, we return the default value."

	| tokens dateStartIndex unknown |
	"The following timestamp strings are supported (source: squeak sources archeological survey):
		<authorname><date><time>. 
		<authorname><date>
		<date><time>
		<date><time><authorname>
		<date><authorname>
		<historical>
	All fields can be separated by spaces or line ends but a separator between authorname and date can be missing as well"
	unknown := anObject.
	aString isEmptyOrNil
		ifTrue: [ ^ unknown ].
	dateStartIndex := (aString indexOf: $/) - 1.	"If there is no / character in the timestamp, we cannot parse a date and return the epoch"
	dateStartIndex = -1
		ifTrue: [ ^ unknown ].
	^ [ 
	"Go the start of the date string (there can be 1 or 2 digits and a space separator can be missing at the front!!)"
	(dateStartIndex >= 2 and: [ (aString at: dateStartIndex - 1) isDigit ])
		ifTrue: [ dateStartIndex := dateStartIndex - 1 ].	"Extract only those tokens that possibly represent date or time"
	tokens := (aString copyFrom: dateStartIndex to: aString size) substrings
		select: [ :token | (token occurrencesOf: $/) = 2 or: [ (token occurrencesOf: $:) = 1 ] ].	"2 tokens is a datetime"
	tokens size = 2
		ifTrue: [ (tokens joinUsing: Character space) asDateAndTime ]
		ifFalse: [ tokens first asDate asDateAndTime ] ]
		on: Exception
		do: [ :e | unknown ]
]

{ #category : #accessing }
RG2ElementDefinition >> package [

	^ self parent package
]
