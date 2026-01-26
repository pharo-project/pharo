"
ASTLiteralNode is an AST node that represents literal values (e.g., #foo, true, 1, etc.), but not literal arrays.

The sourceText field is needed for the formatter for the correct printing of strings vs symbols. If we just call
value asString, both a string and a symbol print itself as a string.

Instance Variables
	value	<Numeric | Symbol | String  | Character>	the literal value I represent
	sourceText <String> the original source text of this literal
"
Class {
	#name : 'OCLiteralValueNode',
	#superclass : 'OCLiteralNode',
	#instVars : [
		'value',
		'sourceText'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
OCLiteralValueNode class >> value: aValue [
	^ self value: aValue start: 0 stop: -1
]

{ #category : 'instance creation' }
OCLiteralValueNode class >> value: aValue start: aPosition stop: anotherPosition [
 	^ self new
		value: aValue start: aPosition stop: anotherPosition;
		yourself
]

{ #category : 'instance creation' }
OCLiteralValueNode class >> value: aValue start: aPosition stop: anotherPosition  source: aString [
 	^ self new
		value: aValue start: aPosition stop: anotherPosition source: aString;
		yourself
]

{ #category : 'comparing' }
OCLiteralValueNode >> = anObject [
	^ super = anObject
		and: [ self value = anObject value
		and: [ self value class = anObject value class
		and: [ self value species = anObject value species ] ] ]
]

{ #category : 'visiting' }
OCLiteralValueNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitLiteralValueNode: self
]

{ #category : 'matching' }
OCLiteralValueNode >> copyInContext: aDictionary [
	^ self class value: self value
]

{ #category : 'evaluating' }
OCLiteralValueNode >> evaluateForContext: aContext [

	^ value
]

{ #category : 'evaluating' }
OCLiteralValueNode >> evaluateForReceiver: aReceicer [

	^ value
]

{ #category : 'testing' }
OCLiteralValueNode >> isString [

	^ value isString and: [ value isSymbol not ]
]

{ #category : 'accessing' }
OCLiteralValueNode >> sourceText [

	sourceText ifNotNil: [ ^ sourceText ].
	value isLiteral ifTrue: [  
		^ String streamContents: [ :aStream |
		 	 value class == Character
			  	ifTrue: [ "Character>>storeOn might return `Character space` (or something else) that is not a literal. So, force the `$x` syntax."
				  aStream nextPut: $$; nextPut: value ]
			  	ifFalse: [ "For other literals, we assume that storeOn returns a valid literal syntactic representation."
				 	 value storeOn: aStream ] ] ] .
		"if we abuse the node to store a normal object (e.g. used for metalinks), do not use #storeOn:"
	^ '''<an unprintable nonliteral value>'''
]

{ #category : 'accessing' }
OCLiteralValueNode >> value [
	^ value
]

{ #category : 'initialization' }
OCLiteralValueNode >> value: aValue start: aPosition stop: anotherPosition [

	value := aValue.
	start := aPosition.
	stop := anotherPosition
]

{ #category : 'initialization' }
OCLiteralValueNode >> value: aValue start: aPosition stop: anotherPosition source: aString [

	self value: aValue start: aPosition stop: anotherPosition.
	sourceText := aString
]
