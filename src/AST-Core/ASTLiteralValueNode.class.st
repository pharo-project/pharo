"
RBLiteralNode is an AST node that represents literal values (e.g., #foo, true, 1, etc.), but not literal arrays.

The sourceText field is needed for the formatter for the correct printing of strings vs symbols. If we just call
value asString, both a string and a symbol print itself as a string.

Instance Variables
	value	<Numeric | Symbol | String  | Character>	the literal value I represent
	sourceText <String> the original source text of this literal
"
Class {
	#name : 'ASTLiteralValueNode',
	#superclass : 'ASTLiteralNode',
	#instVars : [
		'value',
		'sourceText'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'class initialization' }
ASTLiteralValueNode class >> initialize [

	self deprecatedAliases: { #ASTLiteralValueNode }
]

{ #category : 'instance creation' }
ASTLiteralValueNode class >> value: aValue [
	^ self value: aValue start: 0 stop: -1
]

{ #category : 'instance creation' }
ASTLiteralValueNode class >> value: aValue start: aPosition stop: anotherPosition [
 	^ self new
		value: aValue start: aPosition stop: anotherPosition;
		yourself
]

{ #category : 'instance creation' }
ASTLiteralValueNode class >> value: aValue start: aPosition stop: anotherPosition  source: aString [
 	^ self new
		value: aValue start: aPosition stop: anotherPosition source: aString;
		yourself
]

{ #category : 'comparing' }
ASTLiteralValueNode >> = anObject [
	^ super = anObject
		and: [ self value = anObject value
		and: [ self value class = anObject value class
		and: [ self value species = anObject value species ] ] ]
]

{ #category : 'visiting' }
ASTLiteralValueNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitLiteralValueNode: self
]

{ #category : 'matching' }
ASTLiteralValueNode >> copyInContext: aDictionary [
	^ self class value: self value
]

{ #category : 'evaluating' }
ASTLiteralValueNode >> evaluateForContext: aContext [

	^ value
]

{ #category : 'evaluating' }
ASTLiteralValueNode >> evaluateForReceiver: aReceicer [

	^ value
]

{ #category : 'testing' }
ASTLiteralValueNode >> isString [

	^ value isString and: [ value isSymbol not ]
]

{ #category : 'accessing' }
ASTLiteralValueNode >> sourceText [

	sourceText ifNotNil: [ ^ sourceText ].
	^ String streamContents: [ :aStream |
		  value class == Character
			  ifTrue: [ "Character>>storeOn might return `Character space` (or something else) that is not a literal. So, force the `$x` syntax."
				  aStream nextPut: $$; nextPut: value ]
			  ifFalse: [ "For other literals, we assume that storeOn returns a valid literal syntactic representation."
				  value storeOn: aStream ] ]
]

{ #category : 'accessing' }
ASTLiteralValueNode >> value [
	^ value
]

{ #category : 'initialization' }
ASTLiteralValueNode >> value: aValue start: aPosition stop: anotherPosition [

	value := aValue.
	start := aPosition.
	stop := anotherPosition
]

{ #category : 'initialization' }
ASTLiteralValueNode >> value: aValue start: aPosition stop: anotherPosition source: aString [

	self value: aValue start: aPosition stop: anotherPosition.
	sourceText := aString
]
