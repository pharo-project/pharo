Extension { #name : #ProtoObject }

{ #category : #'*OpalCompiler-Core' }
ProtoObject >> mustBeBooleanCompileExpression: context andCache: cache [
	"Permits to redefine methods inlined by compiler.
	Take the ast node corresponding to the mustBeBoolean error, compile it on the fly and executes it as a DoIt. Then resume the execution of the context."

	| sendNode methodNode pc method pcAfterJump |

	"get the message send node that triggered mustBeBoolean"
	pc := context pc - 1.
	sendNode := context sourceNode sourceNodeForPC: pc.
	"Rewrite non-local returns to return to the correct context from send"
	RBParseTreeRewriter new 
		replace: '^ ``@value' with: 'ThisContext home return: ``@value';
		executeTree: sendNode.
	"Build doit node to perform send unoptimized"
	methodNode := sendNode copy asDoitForContext: context.
	"Keep same compilation context as the sender node's"
	methodNode compilationContext: sendNode methodNode compilationContext copy.
	"Disable inlining so the message send will be unoptimized"
	methodNode compilationContext compilerOptions: #(- optionInlineIf optionInlineAndOr optionInlineWhile).
	"Generate the method"	
	OCASTSemanticCleaner clean: methodNode.
	method := methodNode generate.
	"store the pc of the instruction following the send when returning from deoptimized code."
	pcAfterJump := sendNode irInstruction nextBytecodeOffsetAfterJump.
	method propertyAt: #mustBeBooleanJump put: pcAfterJump.
	"cache the method we just created"
	cache at: pc put: method.
	^method
	
	
]

{ #category : #'*OpalCompiler-Core' }
ProtoObject >> mustBeBooleanDeOptimizeIn: context [
	"Permits to redefine methods inlined by compiler.
	Take the ast node corresponding to the mustBeBoolean error, compile it on the fly and executes it as a DoIt. Then resume the execution of the context.
	the generated DoIts are cached in the calling method"

	| ret cache method |

	cache := context method propertyAt: #mustBeBooleanCache ifAbsentPut: [ IdentityDictionary new ].
	"compile a doit method for the unoptimized expression"
	method := cache at: (context pc - 1) ifAbsent: [self mustBeBooleanCompileExpression: context andCache: cache ].
	"Execute the generated method with the pc still at the optimzized block so that the lookUp can read variables defined in the optimized block"
	ret := context receiver withArgs: {context} executeMethod: method.
   	"resume the context at the instruction following the send when returning from deoptimized code."
   context pc: (method propertyAt: #mustBeBooleanJump).
	^ret.
]
