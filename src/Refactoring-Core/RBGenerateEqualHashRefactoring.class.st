"
I am a refactoring for generating #hash and #= comparing methods.

For example, a Class with three instance methods inst1-inst3

RBGenerateEqualHashRefactoring model:RBNamespace new className: ClassS variables: { #inst1 . #inst2 . #inst3 }.

will create:
a #hash method 
hash
	""Answer an integer value that is related to the identity of the receiver.""

	^ inst1 hash bitXor: (inst2 hash bitXor: inst3 hash)
	
and a #= method
= anObject
	""Answer whether the receiver and anObject represent the same object.""

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ inst1 = anObject inst1
		and: [ inst2 = anObject inst2 and: [ inst3 = anObject inst3 ] ]

and any instvar accessor for the  instance variables used by method #=.

"
Class {
	#name : #RBGenerateEqualHashRefactoring,
	#superclass : #RBClassRefactoring,
	#instVars : [
		'variables'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBGenerateEqualHashRefactoring class >> className: aClass variables: anArray [
	^ (self className: aClass) variables: anArray
]

{ #category : #'instance creation' }
RBGenerateEqualHashRefactoring class >> model: aNamespace className: aClass variables: anArray [
	^ (self model: aNamespace className: aClass) variables: anArray
]

{ #category : #transforming }
RBGenerateEqualHashRefactoring >> accessorForVariable: aString [
	| refactoring |
	refactoring := RBCreateAccessorsForVariableRefactoring
		model: self model
		variable: aString
		class: self theClass
		classVariable: false.
	refactoring createGetterAccessor.
	^ refactoring getterMethod
]

{ #category : #transforming }
RBGenerateEqualHashRefactoring >> compileEqual [
	| method statement comparison |
	method := self parserClass
		parseMethod:
			'= anObject
		"Answer whether the receiver and anObject represent the same object."

		self == anObject ifTrue: [ ^ true ].
		self class = anObject class ifFalse: [ ^ false ]'.
	statement := nil.
	variables reversed
		do: [ :each | 
			| accessor |
			accessor := self accessorForVariable: each.
			comparison := RBMessageNode
				receiver: (RBVariableNode named: each)
				selector: #=
				arguments:
					(Array
						with:
							(RBMessageNode
								receiver: (RBVariableNode named: 'anObject')
								selector: accessor)).
			statement := statement
				ifNil: [ comparison ]
				ifNotNil: [ RBMessageNode
						receiver: comparison
						selector: #and:
						arguments:
							(Array
								with:
									(RBBlockNode
										body: (RBSequenceNode statements: (Array with: statement)))) ] ].
	method
		addNode: statement;
		addReturn.
	self theClass compile: method formattedCode classified: #(comparing)
]

{ #category : #transforming }
RBGenerateEqualHashRefactoring >> compileHash [
	| method statement hash |
	method := self parserClass
		parseMethod:
			'hash
		"Answer an integer value that is related to the identity of the receiver."'.
	statement := nil.
	variables reversed
		do: [ :each | 
			hash := RBMessageNode
				receiver: (RBVariableNode named: each)
				selector: #hash.
			statement := statement
				ifNil: [ hash ]
				ifNotNil: [ RBMessageNode
						receiver: hash
						selector: #bitXor:
						arguments: (Array with: statement) ] ].
	method
		addNode: statement;
		addReturn.
	self theClass compile: method formattedCode classified: #(comparing)
]

{ #category : #preconditions }
RBGenerateEqualHashRefactoring >> preconditions [
	^ variables
		inject: RBCondition empty
		into: [ :condition :variable | condition & (RBCondition definesInstanceVariable: variable in: self theClass) ]
]

{ #category : #accessing }
RBGenerateEqualHashRefactoring >> theClass [
	^ (self classObjectFor: className) instanceSide
]

{ #category : #transforming }
RBGenerateEqualHashRefactoring >> transform [
	self compileHash.
	self compileEqual
]

{ #category : #accessing }
RBGenerateEqualHashRefactoring >> variables: anArray [
	variables := anArray
]
