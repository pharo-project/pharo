"
I am a transformation for generating `#hash` and `#=` comparing methods.
Note that this transformation does not check whether an `hash` or `=`  method already exists.


For example, a Class with three instance methods inst1-inst3

```
RBGenerateEqualHashRefactoring 
	model: RBNamespace new 
	className: ClassS 
	variables: { #inst1 . #inst2 . #inst3 }.
```
will create:
a `#hash` method 

```
hash
	""Answer an integer value that is related to the identity of the receiver.""

	^ inst1 hash bitXor: (inst2 hash bitXor: inst3 hash)
```
	
and a `#=` method

```
= anObject
	""Answer whether the receiver and anObject represent the same object.""

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ inst1 = anObject inst1
		and: [ inst2 = anObject inst2 and: [ inst3 = anObject inst3 ] ]
```

and any instvar accessor for the  instance variables used by method `#=`.

"
Class {
	#name : 'ReGenerateEqualHashTransformation',
	#superclass : 'ReGenerateAbstractTransformation',
	#category : 'Refactoring-Core-Transformation',
	#package : 'Refactoring-Core',
	#tag : 'Transformation'
}

{ #category : 'displaying' }
ReGenerateEqualHashTransformation class >> basicMenuItemString [

	^ 'Generate equal and hash'
]

{ #category : 'instance creation' }
ReGenerateEqualHashTransformation class >> className: aClass variables: anArray [
	^ (self className: aClass) variables: anArray
]

{ #category : 'testing' }
ReGenerateEqualHashTransformation class >> isTransformation [

	^ true
]

{ #category : 'instance creation' }
ReGenerateEqualHashTransformation class >> model: aNamespace className: aClass variables: anArray [
	^ (self model: aNamespace className: aClass) variables: anArray
]

{ #category : 'transforming' }
ReGenerateEqualHashTransformation >> accessorForVariable: aString [

	| refactoring |
	refactoring := RBCreateAccessorsForVariableTransformation
		               model: self model
		               variable: aString
		               class: self theClass
		               classVariable: false.
	refactoring createGetterAccessor.
	^ refactoring getterMethodName
]

{ #category : 'transforming' }
ReGenerateEqualHashTransformation >> compileEqual [
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
	self generateChangesFor:
		(RBAddMethodTransformation
			sourceCode: method formattedCode
			in: self theClass
			withProtocol: #comparing)
]

{ #category : 'transforming' }
ReGenerateEqualHashTransformation >> compileHash [
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
	self generateChangesFor:
		(RBAddMethodTransformation
			sourceCode: method formattedCode
			in: self theClass
			withProtocol: #comparing)
]

{ #category : 'transforming' }
ReGenerateEqualHashTransformation >> privateTransform [
	self compileHash.
	self compileEqual
]
