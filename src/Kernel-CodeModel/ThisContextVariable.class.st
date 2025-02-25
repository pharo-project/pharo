"
I model thisContext
"
Class {
	#name : 'ThisContextVariable',
	#superclass : 'PseudoVariable',
	#category : 'Kernel-CodeModel-Variables',
	#package : 'Kernel-CodeModel',
	#tag : 'Variables'
}

{ #category : 'accessing' }
ThisContextVariable class >> variableName [
	^#thisContext
]

{ #category : 'visiting' }
ThisContextVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitThisContextNode: aNode
]

{ #category : 'code generation' }
ThisContextVariable >> emitValue: methodBuilder [

	methodBuilder pushThisContext
]

{ #category : 'testing' }
ThisContextVariable >> isThisContextVariable [
	^true
]

{ #category : 'debugging' }
ThisContextVariable >> readInContext: aContext [
	^aContext
]

{ #category : 'queries' }
ThisContextVariable >> usingMethods [
	^ environment allMethods select: [ :method |
		  method readsThisContext ]
]
