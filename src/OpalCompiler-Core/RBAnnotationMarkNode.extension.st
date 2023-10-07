Extension { #name : 'RBAnnotationMarkNode' }

{ #category : '*OpalCompiler-Core' }
RBAnnotationMarkNode >> emitValue: methodBuilder [

	emitValueBlock ifNotNil: [ ^ emitValueBlock value: methodBuilder ].
	methodBuilder
		pushLiteralVariable: RuntimeSyntaxError binding;
		pushLiteral: 'Unhandled annotation';
		send: #signalSyntaxError:
]

{ #category : '*OpalCompiler-Core' }
RBAnnotationMarkNode >> emitValueBlock [

	^ emitValueBlock
]

{ #category : '*OpalCompiler-Core' }
RBAnnotationMarkNode >> emitValueBlock: anObject [

	emitValueBlock := anObject
]
