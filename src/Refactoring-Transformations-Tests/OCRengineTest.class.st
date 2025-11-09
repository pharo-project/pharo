Class {
	#name : 'OCRengineTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Transformations-Tests-AST',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'AST'
}

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
