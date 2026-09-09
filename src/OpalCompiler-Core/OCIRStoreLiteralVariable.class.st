"
store literal variable
"
Class {
	#name : 'OCIRStoreLiteralVariable',
	#superclass : 'OCIRLiteralVariableAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRStoreLiteralVariable >> accept: aVisitor [
	^ aVisitor visitStoreLiteralVariable: self
]

{ #category : 'testing' }
OCIRStoreLiteralVariable >> isStore [
	^ true
]

{ #category : 'accessing' }
OCIRStoreLiteralVariable >> popIntoNode [
	^ OCIRPopIntoLiteralVariable new
		association: association;
		yourself
]
