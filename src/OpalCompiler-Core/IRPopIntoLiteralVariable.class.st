"
pop into literal variable
"
Class {
	#name : 'IRPopIntoLiteralVariable',
	#superclass : 'IRLiteralVariableAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPopIntoLiteralVariable >> accept: arg1 [

	^ arg1 visitPopIntoLiteralVariable: self
]
