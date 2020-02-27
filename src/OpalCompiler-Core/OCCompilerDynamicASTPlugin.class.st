"
Look at the OCCompilerASTPlugin class comment for information about compiler plugins.

I allow the dynamic creation of compiler plugins.

Instanciate me using """"newFromTransformBlock: aBlock andPriority: aPriority"""" to get a compiler plugin you can add to an OpalCompiler instance with OpalCompiler>>addPlugin:.
- aBlock must be a block of the form [ :ast | do something to ast ] that returns the modified ast.
- aPriority is the priority the created plugin should have

Check out my test class (OCCompilerDynamicASTPluginTest) for a usage example.
"
Class {
	#name : #OCCompilerDynamicASTPlugin,
	#superclass : #Object,
	#instVars : [
		'priority',
		'transformBlock'
	],
	#category : #'OpalCompiler-Core-Translator'
}

{ #category : #'instance creation' }
OCCompilerDynamicASTPlugin class >> newFromTransformBlock: aBlock andPriority: aPriority [
	^ self new
		transformBlock: aBlock;
		priority: aPriority.
]

{ #category : #accessing }
OCCompilerDynamicASTPlugin >> priority [
	^ priority
]

{ #category : #accessing }
OCCompilerDynamicASTPlugin >> priority: anObject [
	priority := anObject
]

{ #category : #accessing }
OCCompilerDynamicASTPlugin >> transform: ast [
	^ transformBlock value: ast copy.
]

{ #category : #accessing }
OCCompilerDynamicASTPlugin >> transformBlock [
	^ transformBlock
]

{ #category : #accessing }
OCCompilerDynamicASTPlugin >> transformBlock: anObject [
	transformBlock := anObject
]
