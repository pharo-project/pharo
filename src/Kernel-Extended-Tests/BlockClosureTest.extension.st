Extension { #name : 'BlockClosureTest' }

{ #category : '*Kernel-Extended-Tests' }
BlockClosureTest >> testPrintOnBlockDefinedInMethodWithoutSourceCode [
	| method block |
	method := OpalCompiler new compile: 'method ^ [ 1 + 2 ]'.
	method removeProperty: #source.
	self deny: method hasSourceCode.
	
	block := method valueWithReceiver: nil.
	"we compare the ast to not depend on pretty printer settings"
	self assert: (OCParser parseExpression: block printString) equals: (OCParser parseExpression: '[ 1 + 2 ]').
]

{ #category : '*Kernel-Extended-Tests' }
BlockClosureTest >> testRunSimulated [
	self assert: (Context runSimulated: aBlock) class equals: Rectangle
]

{ #category : '*Kernel-Extended-Tests' }
BlockClosureTest >> testSourceNodeOptimized [
	| block |
	block := [ :ctx | [ ctx atEnd ] whileTrue: [ 1 + 2 ] ].
	self assert: block sourceNode printString equals: 'OCBlockNode([ :ctx | [ ctx atEnd ] whileTrue: [ 1 + 2 ] ])'
]

{ #category : '*Kernel-Extended-Tests' }
BlockClosureTest >> testTallyInstructions [

	| expected |
	expected := Smalltalk compiler compilationContext
		            optionCleanBlockClosure
		            ifTrue: [ 26 ]
		            ifFalse: [ 27 ].
	self
		assert: (Context tallyInstructions: aBlock) size
		equals: expected
]

{ #category : '*Kernel-Extended-Tests' }
BlockClosureTest >> testTallyMethods [
	self assert: (Context tallyMethods: aBlock) size equals: 7
]
