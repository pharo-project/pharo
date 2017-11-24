"
push inst var
"
Class {
	#name : #IRPushInstVar,
	#superclass : #IRInstVarAccess,
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushInstVar >> accept: aVisitor [
	^ aVisitor visitPushInstVar: self
]

{ #category : #testing }
IRPushInstVar >> canBeQuickReturn [
	^ true
]

{ #category : #accessing }
IRPushInstVar >> quickRetNode [
	^ IRReturnInstVar new
		index: index;
		yourself
]
