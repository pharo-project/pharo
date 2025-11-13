Class {
	#name : 'OCRengineTest',
	#superclass : 'TestCase',
	#instVars : [
		'usedInstanceVariable'
	],
	#category : 'Refactoring-Transformations-Tests-AST',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'AST'
}

{ #category : 'tests - defines' }
OCRengineTest >> testExactNodeDefinesArg [

	| ast |
	ast := OCParser parseMethod: 'defines: tmp

	| each |
	each := 2.
	^ each 
	
	'.
	self assert: (ast exactNodeDefines: 'tmp')
]

{ #category : 'tests - defines' }
OCRengineTest >> testExactNodeDefinesTemporaryInBlockViaBody [
	"ExactNodeDefines does not look in children or body. Therefore users should use explicit body."
	
	| ast |
	ast := OCParser parseMethod: 'defines

	[  
	| each |
	each := 2.
	^ each 
	]
	'.
	
	"why why the temporaries is not defining the variable?????"
	self assert: (ast body children first body exactNodeDefines: 'each')
]

{ #category : 'tests - defines' }
OCRengineTest >> testExactNodeDefinesTemporaryViaBody [
	"ExactNodeDefines does not look in children or body. Therefore users should use explicit body."
	
	| ast |
	ast := OCParser parseMethod: 'defines

	| each |
	each := 2.
	^ each 
	
	'.
	
	"why why the temporaries is not defining the variable?????"
	self assert: (ast body exactNodeDefines: 'each')
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesABlockArgInReturnRead [

	| ast |
	ast := OCParser parseMethod: 'references

[ :each | ^ each ]	
	'.
	self assert: (ast references: 'each').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesABlockArgRead [

	| ast |
	ast := OCParser parseMethod: 'references

[ :each | each ]	
	'.
	self assert: (ast references: 'each').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesABlockTmpAssignment [

	| ast |
	ast := OCParser parseMethod: 'references

[ :each | | tmp |
	tmp := 2 ]	
	'.
	self assert: (ast references: 'tmp').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesABlockTmpRead [

	| ast |
	ast := OCParser parseMethod: 'references

[ :each | | tmp |
	tmp ]	
	'.
	self assert: (ast references: 'tmp').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesABlockTmpReturn [

	| ast |
	ast := OCParser parseMethod: 'references

[ :each | | tmp |
	^ tmp ]	
	'.
	self assert: (ast references: 'tmp').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesATemporaryViaAssignment [

	| ast |
	ast := OCParser parseMethod: 'referencesTemp | tmp | tmp := 2'.
	self assert: (ast references: 'tmp')

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesATemporaryViaRead [

	| ast |
	ast := OCParser parseMethod: 'referencesTemp | tmp y | y := tmp'.
	self assert: (ast references: 'tmp').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesATemporaryViaReadInBlock [

	| ast |
	ast := OCParser parseMethod: 'referencesTemp | tmp | [ :each |  tmp ]'.
	self assert: (ast references: 'tmp').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesATemporaryViaReadInBlock2 [

	| ast |
	ast := OCParser parseMethod: 'referencesTemp | tmp |
	2 isZero ifTrue: [ :each |  tmp ]'.
	self assert: (ast references: 'tmp').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesATemporaryViaReadInDynamicArray [

	| ast |
	ast := OCParser parseMethod: 'referencesTemp | tmp |
	{ tmp . 2 } '.
	self assert: (ast references: 'tmp').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesATemporaryViaReadInReturn [

	| ast |
	ast := OCParser parseMethod: 'referencesTemp | tmp | ^ tmp'.
	self assert: (ast references: 'tmp').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesAnArg [

	| ast |
	ast := OCParser parseMethod: 'references: arg  arg'.
	self assert: (ast references: 'arg').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesAnArgInReturn [

	| ast |
	ast := OCParser parseMethod: 'references: arg ^ arg'.
	self assert: (ast references: 'arg').

	
]

{ #category : 'tests - references' }
OCRengineTest >> testReferencesToAnInstanceVariable [

	| ast |
	ast := (self class >> #usingTheUsedInstanceVariable) ast.
	self assert: (ast references: 'usedInstanceVariable' ).

	
]

{ #category : 'tests - whodefines' }
OCRengineTest >> testWhichUNodesDefinesBlockArg [

	| ast |
	self skip.
	ast := OCParser parseMethod: 'whoDefines

[ :each | ^ each ]	
	'.
	self assert: (ast body children first body children first value whichUpNodeDefines: 'each') class equals: OCBlockNode

	
]

{ #category : 'tests - whodefines' }
OCRengineTest >> testWhichUpNodeDefines [

	| ast |
	ast := OCParser parseMethod: 'whoDefines

	| each |
	each := 2.
	^ each 
	
	'.
	
	self assert: (ast children first children third value whichUpNodeDefines: 'each') class equals: OCSequenceNode

	
]

{ #category : 'tests - whodefines' }
OCRengineTest >> testWhoDefinesTemporary [

	| ast |
	ast := OCParser parseMethod: 'whoDefines

	| each |
	each := 2.
	^ each 
	
	'.
	
	self assert: (ast children first children third value whichUpNodeDefines: 'each') class equals: OCSequenceNode

	
]

{ #category : 'tests - references' }
OCRengineTest >> usingTheUsedInstanceVariable [

	usedInstanceVariable := 33.
	^ usedInstanceVariable 	
]
