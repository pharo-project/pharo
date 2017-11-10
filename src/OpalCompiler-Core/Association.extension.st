Extension { #name : #Association }

{ #category : #'*OpalCompiler-Core' }
Association >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable: self.


]

{ #category : #'*OpalCompiler-Core' }
Association >> emitValue: methodBuilder [

	methodBuilder pushLiteralVariable: self.
]
