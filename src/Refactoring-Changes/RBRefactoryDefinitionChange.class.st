"
I am the baseclass for a ""class definition change"", a change that will create a class or modify the definition of an existing class.
I hold the class definition string and a controller used by the compiler when compiling the class definition for notifying about the
compilation result. My subclass define the kind of class definition I can add, a class, a metaclass a trait or a class trait.

My instance variable ""definedClass"" is the new class object.
"
Class {
	#name : 'RBRefactoryDefinitionChange',
	#superclass : 'RBRefactoryClassChange',
	#instVars : [
		'definition',
		'definedClass'
	],
	#category : 'Refactoring-Changes-Changes-Classes',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Classes'
}

{ #category : 'instance creation' }
RBRefactoryDefinitionChange class >> definition: aString [

	| parseTree context |
	parseTree := RBParser
		             parseExpression: aString
		             onError: [ :err :pos |
		             ^ self error: 'Invalid definition string' ].
	context := SmallDictionary new.
	RBRefactoryDefinitionChange allSubclassesDo: [ :class |
		class definitionPatterns do: [ :pattern |
			((RBParser parseRewriteExpression: pattern)
				 match: parseTree
				 inContext: context empty) ifTrue: [
				^ class definition: aString context: context ] ] ].
	^ self error: 'Invalid definition:' , aString
]

{ #category : 'private' }
RBRefactoryDefinitionChange class >> definition: aString context: aDictionary [

	| dictionary |
	dictionary := SmallDictionary new.
	aDictionary keysAndValuesDo: [ :key :node |
		dictionary at: key formattedCode put: (node isVariable
				 ifTrue: [ node name ]
				 ifFalse: [
					 node isLiteralNode
						 ifTrue: [ node value ]
						 ifFalse: [ node ] ]) ].
	^ self new
		  definition: aString;
		  fillOutDefinition: dictionary;
		  yourself
]

{ #category : 'private' }
RBRefactoryDefinitionChange class >> definitionPatterns [

	self subclassResponsibility
]

{ #category : 'comparing' }
RBRefactoryDefinitionChange >> = aDefinitionChange [

	^ self class = aDefinitionChange class and: [
		  self definition = aDefinitionChange definition ]
]

{ #category : 'printing' }
RBRefactoryDefinitionChange >> changeString [

	^ 'Define ' , self displayClassName
]

{ #category : 'accessing' }
RBRefactoryDefinitionChange >> definedClass [

	^ definedClass
]

{ #category : 'accessing' }
RBRefactoryDefinitionChange >> definition [

	^ definition
]

{ #category : 'initialization' }
RBRefactoryDefinitionChange >> definition: aString [

	isMeta := false.
	definition := aString
]

{ #category : 'private' }
RBRefactoryDefinitionChange >> definitionClass [

	self subclassResponsibility
]

{ #category : 'initialization' }
RBRefactoryDefinitionChange >> fillOutDefinition: aDictionary [

	self subclassResponsibility
]

{ #category : 'private' }
RBRefactoryDefinitionChange >> generateChanges [

	definedClass := self definitionClass environment defineClass: self definition
]

{ #category : 'comparing' }
RBRefactoryDefinitionChange >> hash [

	^ definition hash
]

{ #category : 'private' }
RBRefactoryDefinitionChange >> namesIn: aString [

	| names scanner token |
	names := OrderedCollection new.
	scanner := RBScanner on: (ReadStream on: aString).
	[ scanner atEnd ] whileFalse: [
		token := scanner next.
		token isIdentifier ifTrue: [ names add: token value ] ].
	^ names asArray
]

{ #category : 'printing' }
RBRefactoryDefinitionChange >> printOn: aStream [

	aStream
		nextPutAll: definition
]
