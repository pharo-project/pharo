"
pop into temp
"
Class {
	#name : 'IRPopIntoTemp',
	#superclass : 'IRTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPopIntoTemp >> accept: arg1 [

	^ arg1 visitPopIntoTemp: self
]

{ #category : 'accessing' }
IRPopIntoTemp >> nextBytecodeOffsetAfterJump [

	^ self sequence last destination last destination first
		  bytecodeOffset
]
