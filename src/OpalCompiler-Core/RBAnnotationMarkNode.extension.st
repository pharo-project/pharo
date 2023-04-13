Extension { #name : #RBAnnotationMarkNode }

{ #category : #'*OpalCompiler-Core' }
RBAnnotationMarkNode >> emitValue: methodBuilder [

	emitValueBlock ifNotNil: [ ^ emitValueBlock value: methodBuilder ].
	methodBuilder
		pushLiteralVariable: Error binding;
		pushLiteral: 'Error: untransformed annotation';
		send: #signal:
]

{ #category : #'*OpalCompiler-Core' }
RBAnnotationMarkNode >> emitValueBlock [

	^ emitValueBlock
]

{ #category : #'*OpalCompiler-Core' }
RBAnnotationMarkNode >> emitValueBlock: anObject [

	emitValueBlock := anObject
]
