"
store literal variable
"
Class {
	#name : #IRStoreLiteralVariable,
	#superclass : #IRLiteralVariableAccess,
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRStoreLiteralVariable >> accept: aVisitor [
	^ aVisitor visitStoreLiteralVariable: self
]

{ #category : #testing }
IRStoreLiteralVariable >> isStore [
	^ true
]

{ #category : #accessing }
IRStoreLiteralVariable >> popIntoNode [
	^ IRPopIntoLiteralVariable new
		association: association;
		yourself
]
