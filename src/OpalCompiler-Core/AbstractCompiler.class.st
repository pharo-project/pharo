"
I define the public API of compilers that can be used as system compilers (e.g. overriding #compiler on the class side).
"
Class {
	#name : #AbstractCompiler,
	#superclass : #Object,
	#classInstVars : [
		'compileWithErrors'
	],
	#category : #'OpalCompiler-Core-FrontEnd'
}

{ #category : #settings }
AbstractCompiler class >> compileWithErrors [
	^compileWithErrors ifNil: [ false ] 
]

{ #category : #settings }
AbstractCompiler class >> compileWithErrors: aBoolean [
	compileWithErrors := aBoolean 
]

{ #category : #settings }
AbstractCompiler class >> compilerSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #compiler)
		label: 'Compiler';
		description: 'All Compiler Settings';
		with: [ (aBuilder setting: #compileWithErrors) 
						label: 'Allow code with syntax errors';
						description: 'If checked then code with syntax errros can be compiled';
						target: self.
					(aBuilder pickOne: #compilerClass)
						target: SmalltalkImage;
						label: 'Default Compiler';
						domainValues: AbstractCompiler subclasses.
					(aBuilder pickOne: #bytecodeBackend)
						target: CompilationContext;
						label: 'Bytecode Backend';
						domainValues:
								{#V3PlusClosures -> EncoderForV3PlusClosures.
								#SistaV1 -> EncoderForSistaV1 }.
					(aBuilder setting: #usesFullBlockClosure)
						target: CompilationContext;
						label: 'FullBlockClosure';
						default: CompilationContext usesFullBlockClosure.]
]

{ #category : #plugins }
AbstractCompiler >> addPlugin: aClass [ 
	"do nothing"
]

{ #category : #accessing }
AbstractCompiler >> class: aClass [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> compilationContext [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> compilationContext: anObject [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> compilationContextClass [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> compilationContextClass: aClass [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> compile [
	self subclassResponsibility
]

{ #category : #'old - public' }
AbstractCompiler >> compile: textOrStream in: aClass classified: aCategory notifying: aRequestor ifFail:  aFailBlock [ 

	^self 
		source: textOrStream;
		class: aClass;
		requestor: aRequestor;
		failBlock: aFailBlock;
		translate
]

{ #category : #'old - public' }
AbstractCompiler >> compile: textOrStream in: aClass notifying: aRequestor ifFail: aFailBlock [ 

	^self 
		source: textOrStream;
		class: aClass;
		requestor: aRequestor;
		failBlock: aFailBlock;
		translate
	

]

{ #category : #'old - public' }
AbstractCompiler >> compileNoPattern: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: aFailBlock [

	^self 
		source: textOrStream;
		class: aClass;
		context: aContext;
		requestor: aRequestor;
		noPattern: true;
		failBlock: aFailBlock;
		translate
]

{ #category : #accessing }
AbstractCompiler >> compiledMethodTrailer: aClass [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> context: aContext [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> decompileMethod: aCompiledMethod [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> encoderClass: aClass [ 
	self compilationContext encoderClass: aClass 
]

{ #category : #accessing }
AbstractCompiler >> environment: anSmallTalkImage [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> evaluate [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> evaluate: textOrString [ 
	
	^self
		source: textOrString;
		evaluate
]

{ #category : #'old - public' }
AbstractCompiler >> evaluate: textOrString for: anObject logged: logFlag [ 

	^self
		source: textOrString;
		logged: logFlag;
		receiver: anObject;
		evaluate
]

{ #category : #'old - public' }
AbstractCompiler >> evaluate: textOrString for: anObject notifying: aController logged: logFlag [

	^ self 
		source: textOrString;
		logged: logFlag;
		receiver: anObject;
		requestor: aController;
		evaluate
	
			
]

{ #category : #'old - public' }
AbstractCompiler >> evaluate: aString in: aContext to: aReceiver [
	
	^self
		source: aString;
		context: aContext;
		receiver: aReceiver;
		failBlock: [^ #failedDoit];
		evaluate
]

{ #category : #'old - public' }
AbstractCompiler >> evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: aFailBlock [ 
	
	^self
		source: textOrStream;
		context: aContext;
		receiver: aReceiver;
		requestor: aRequestor;
		failBlock: aFailBlock;
		evaluate
]

{ #category : #'old - public' }
AbstractCompiler >> evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: aFailBlock logged: logFlag [
	
	^self
		source: textOrStream;
		context: aContext;
		receiver: aReceiver;
		requestor: aRequestor;
		failBlock: aFailBlock;
		logged: logFlag;
		evaluate
]

{ #category : #'old - public' }
AbstractCompiler >> evaluate: textOrString logged: logFlag [ 

	^ self
		source: textOrString;
		logged: logFlag;
		evaluate
	
			
]

{ #category : #'old - public' }
AbstractCompiler >> evaluate: textOrString notifying: aController logged: logFlag [ 

	^ self
		source: textOrString;
		logged: logFlag;
		requestor: aController;
		evaluate
	
]

{ #category : #accessing }
AbstractCompiler >> failBlock: aBlock [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> format [
	self subclassResponsibility
]

{ #category : #'old - public' }
AbstractCompiler >> format: textOrStream in: aClass notifying: aRequestor [
	^self
		source: textOrStream;
		class: aClass;
		requestor: aRequestor;
		format
		
]

{ #category : #accessing }
AbstractCompiler >> logged: aBoolean [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> noPattern: aBoolean [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> options: anArray [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> parse [
	self subclassResponsibility
]

{ #category : #'old - public' }
AbstractCompiler >> parse: aString class: aClass [
	^self 
		source: aString;
		class: aClass;
		parse
]

{ #category : #'old - public' }
AbstractCompiler >> parse: aString class: aClass noPattern: aBoolean context: aContext notifying: req ifFail: aBlock [
	"Backwards compatibilty"
	
	^self 
		source: aString;
		class: aClass;
		noPattern: aBoolean;
		context: aContext;
		requestor: req;
		failBlock: aBlock;
		translate.
]

{ #category : #'old - public' }
AbstractCompiler >> parse: textOrStream in: aClass notifying: req [

	^self 
		source: textOrStream;
		class: aClass;
		requestor: req;
		translate.
]

{ #category : #'public access' }
AbstractCompiler >> parseLiterals: aString [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> parseSelector: aString [ 
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> receiver: anObject [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> requestor: aRequestor [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> requestorScopeClass: aClass [ 
	"clients can set their own subclass of OCRequestorScope if needed"
	self compilationContext requestorScopeClass: aClass 
]

{ #category : #accessing }
AbstractCompiler >> source: aString [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> translate [
	self subclassResponsibility
]
