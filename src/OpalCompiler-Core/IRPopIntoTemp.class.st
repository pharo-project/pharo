"
pop into temp
"
Class {
	#name : #IRPopIntoTemp,
	#superclass : #IRTempAccess,
	#category : 'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPopIntoTemp >> accept: aVisitor [
	^ aVisitor visitPopIntoTemp: self
]
