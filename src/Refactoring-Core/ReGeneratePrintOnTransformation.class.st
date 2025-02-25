"
I am a transformation that generates a printOn: method. 

You can specify which of my instance variables should be used for generating the printOn: method.

For example: 

```
RBGeneratePrintOnTransformation 
	model: RBNamespace new 
	className: ClassS 
	variables: { #inst1 . #inst2 . #inst3 }
```

will generate

```

printOn: aStream
		""Generate a string representation of the receiver based on its instance variables.""
		
		super printOn: aStream
		aStream nextPutAll: 'inst1'; print: inst1.
		aStream nextPutAll: 'inst2'; print: inst2.
		aStream nextPutAll: 'inst3'; print: inst3
				
```
"
Class {
	#name : 'ReGeneratePrintOnTransformation',
	#superclass : 'ReGenerateAbstractTransformation',
	#category : 'Refactoring-Core-Transformation',
	#package : 'Refactoring-Core',
	#tag : 'Transformation'
}

{ #category : 'displaying' }
ReGeneratePrintOnTransformation class >> basicMenuItemString [

	^ 'Generate printOn:'
]

{ #category : 'instance creation' }
ReGeneratePrintOnTransformation class >> className: aClass variables: anArray [
	^ (self className: aClass) variables: anArray
]

{ #category : 'testing' }
ReGeneratePrintOnTransformation class >> isTransformation [

	^ true
]

{ #category : 'instance creation' }
ReGeneratePrintOnTransformation class >> model: aNamespace className: aClass variables: anArray [
	^ (self model: aNamespace className: aClass) variables: anArray
]

{ #category : 'transforming' }
ReGeneratePrintOnTransformation >> privateTransform [
	| method |
	method := self parserClass
		parseMethod:
			'printOn: aStream
		"Generate a string representation of the receiver based on its instance variables."

		super printOn: aStream'.
	variables
		do: [ :each |
			method body
				addNode:
					(self parserClass
						parseExpression:
							('aStream nextPutAll: '' <1s>: ''; print: <1s>' expandMacrosWith: each)) ].
	self generateChangesFor:
		(RBAddMethodTransformation
			sourceCode: method formattedCode
			in: self theClass
			withProtocol: #printing)
]
