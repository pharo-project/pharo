"
Look at the `OCASTCompilerPlugin` class comment for information about compiler plugins.

I allow the dynamic creation of compiler plugins.

Instanciate me using """"newFromTransformBlock: aBlock andPriority: aPriority"""" to get a compiler plugin you can add to an OpalCompiler instance with OpalCompiler>>addPlugin:.
- aBlock must be a block of the form [ :ast | do something to ast ] that returns the modified ast.
- aPriority is the priority the created plugin should have

Check out my test class (`OCDynamicASTCompilerPluginTest`) for a usage example.
"
Class {
	#name : #OCDynamicASTCompilerPlugin,
	#superclass : #OCASTCompilerPlugin,
	#instVars : [
		#priority,
		#transformBlock
	],
	#category : #'OpalCompiler-Core-Plugins'
}

{ #category : #'instance creation' }
OCDynamicASTCompilerPlugin class >> newFromTransformBlock: aBlock [
	"Return a new instance of the receiver using the given AST transformation block and default priority."

	^ self new
		transformBlock: aBlock;
		yourself
]

{ #category : #'instance creation' }
OCDynamicASTCompilerPlugin class >> newFromTransformBlock: aBlock andPriority: aPriority [
	"Return a new instance of the receiver using the given AST transformation block and the given priority."

	^ (self newFromTransformBlock: aBlock)
			priority: aPriority;
			yourself
]

{ #category : #initialization }
OCDynamicASTCompilerPlugin >> initialize [

	super initialize.
	self priority: self class defaultPriority
]

{ #category : #accessing }
OCDynamicASTCompilerPlugin >> priority [

	^ priority
]

{ #category : #accessing }
OCDynamicASTCompilerPlugin >> priority: anObject [

	priority := anObject
]

{ #category : #transforming }
OCDynamicASTCompilerPlugin >> transform: ast [

	^ transformBlock value: ast copy
]

{ #category : #accessing }
OCDynamicASTCompilerPlugin >> transformBlock [

	^ transformBlock
]

{ #category : #accessing }
OCDynamicASTCompilerPlugin >> transformBlock: anObject [

	transformBlock := anObject
]
