"
push inst var
"
Class {
	#name : 'OCIRPushInstVar',
	#superclass : 'OCIRInstVarAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushInstVar >> accept: aVisitor [
	^ aVisitor visitPushInstVar: self
]

{ #category : 'testing' }
OCIRPushInstVar >> canBeQuickReturn [
	^ true
]

{ #category : 'testing' }
OCIRPushInstVar >> isRemovableByPop [

	^ true
]

{ #category : 'accessing' }
OCIRPushInstVar >> quickRetNode [
	^ OCIRReturnInstVar new
		index: index;
		yourself
]
