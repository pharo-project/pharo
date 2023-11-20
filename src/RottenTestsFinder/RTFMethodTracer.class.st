"
I allow to trace the fact that a method was called or not during execution.

I am used to check if a test calls one of the assert methods during its execution.
"
Class {
	#name : 'RTFMethodTracer',
	#superclass : 'ProtoObject',
	#instVars : [
		'hasRun',
		'methodClass',
		'selector',
		'method'
	],
	#category : 'RottenTestsFinder-Tracers',
	#package : 'RottenTestsFinder',
	#tag : 'Tracers'
}

{ #category : 'testing' }
RTFMethodTracer class >> canInstall: aCompileMethod [
	"Returns wether the method tracer can be installed on a given CompileMethod or not."
	self classesBlackList do: [ :each | (aCompileMethod methodClass == each) ifTrue: [ ^ false ] ].

	self methodsBlackList pairsDo: [ :aClass :aSelector |
		(aCompileMethod methodClass == aClass and: [ aCompileMethod selector == aSelector ])
			ifTrue: [ ^ false ] ].

	^ true
]

{ #category : 'accessing' }
RTFMethodTracer class >> classesBlackList [
	^ { self . self class }
]

{ #category : 'accessing' }
RTFMethodTracer class >> methodsBlackList [
	^ {
		ArrayedCollection. #size.
		SmallInteger. #'\\'.
		ProtoObject. #isNil.
		ProtoObject. #basicIdentityHash.
		Object. #basicAt:.
		Object. #at:put:.
		Behavior. #methodDict.
		HashedCollection. #findElementOrNil:.
		MethodDictionary. #at:put:.
		MethodDictionary. #scanFor:.
		MethodDictionary. #swap:with:.
		BlockClosure. #value:.
		HashedCollection. #fullCheck.
		LargeInteger. #+.
		Magnitude. #max:.
		Object. #at:.
		ProtoObject. #withArgs:executeMethod:.
		SequenceableCollection. #do:.
		SmallInteger. #'//'.
		SmallInteger. #*.
		SmallInteger. #-.
		Symbol. #flushCache.
		Number. #isZero.
		Object. #shallowCopy.
		OrderedCollection. #do:.
		ProtoObject. #initialize.
		SequenceableCollection. #first.
		SmallInteger. #=.
		UndefinedObject. #notNil.
	}
]

{ #category : 'instance creation' }
RTFMethodTracer class >> on: aCompiledMethod [
	"Initialize the method tracer on aCompiledMethod and returns the method tracer.
	 Do not install the tracer. You need to explicitely call #install on the instance returned to install it.
	"
	(self canInstall: aCompiledMethod)
		ifFalse: [ self error: ('It is forbidden to install myself on a {1}' format: {aCompiledMethod asString}) ].
	^ self basicNew initializeOn: aCompiledMethod
]

{ #category : 'reflective operations' }
RTFMethodTracer >> doesNotUnderstand: aMessage [
	"Messages not understood by myself are forwarded to the CompiledMethod I hold."
	^ method perform: aMessage selector withArguments: aMessage arguments
]

{ #category : 'testing' }
RTFMethodTracer >> hasRun [
	^ hasRun
]

{ #category : 'initialization' }
RTFMethodTracer >> initializeOn: aCompiledMethod [

	hasRun := false.
	method := aCompiledMethod.
	methodClass := aCompiledMethod methodClass.
	selector := aCompiledMethod selector
]

{ #category : 'actions' }
RTFMethodTracer >> install [
	"Install myself instead of the CompiledMethod in the class holding it.
	 This way, when sending a message to the CompiledMethod (to execute it for example)
	 I am notified and I can remember that the method was run.
	"
	methodClass methodDict
		at: selector
		put: self
]

{ #category : 'private' }
RTFMethodTracer >> mark [
	hasRun := true
]

{ #category : 'accessing' }
RTFMethodTracer >> method [
	^ method
]

{ #category : 'evaluation' }
RTFMethodTracer >> run: aSelector with: anArray in: aReceiver [
	self mark.
	^ aReceiver withArgs: anArray executeMethod: method
]

{ #category : 'actions' }
RTFMethodTracer >> uninstall [
	"Put the CompiledMethod I replaced back to its place.
	 After this method has been executed, I do not receive CompiledMethod's
	 messages before forwarding them to it anymore.
	"
	methodClass methodDict
		at: selector
		put: method
]

{ #category : 'actions' }
RTFMethodTracer >> unmark [
	"Forget the fact that the method was run."
	hasRun := false
]
