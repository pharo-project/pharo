Extension { #name : 'ASTAnnotationMarkNode' }

{ #category : '*OpalCompiler-Core' }
ASTAnnotationMarkNode >> emitValue: methodBuilder [

	emitValueBlock ifNotNil: [ ^ emitValueBlock value: methodBuilder ].
	methodBuilder
		pushLiteralVariable: RuntimeSyntaxError binding;
		pushLiteral: 'Unhandled annotation';
		send: #signalSyntaxError:
]

{ #category : '*OpalCompiler-Core' }
ASTAnnotationMarkNode >> emitValueBlock [

	^ emitValueBlock
]

{ #category : '*OpalCompiler-Core' }
ASTAnnotationMarkNode >> emitValueBlock: anObject [

	emitValueBlock := anObject
]
