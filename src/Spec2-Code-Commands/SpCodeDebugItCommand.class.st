Class {
	#name : #SpCodeDebugItCommand,
	#superclass : #SpCodeSelectionCommand,
	#category : #'Spec2-Code-Commands'
}

{ #category : #defaults }
SpCodeDebugItCommand class >> defaultIconName [
	^#smallDebug
]

{ #category : #default }
SpCodeDebugItCommand class >> defaultName [ 

	^ 'Debug it'
]

{ #category : #default }
SpCodeDebugItCommand class >> defaultShortcutKey [

	^ $d shift command mac 
		| $d shift control win
		| $d shift control unix
]

{ #category : #private }
SpCodeDebugItCommand >> compile: aStream for: anObject in: evalContext [
	| methodClass |

	methodClass := evalContext
		ifNil: [ anObject class ]
		ifNotNil: [ evalContext methodClass ].

	^ context class compiler
		source: aStream;
		class: methodClass;
		context: evalContext;
		requestor: nil;
		noPattern: true;
		failBlock: [ ^ nil ];
		compile
]

{ #category : #private }
SpCodeDebugItCommand >> debug: aStream [
	| method doItReceiver doItContext |

	(self context respondsTo: #doItReceiver)
		ifTrue: [ 
			doItReceiver := self context doItReceiver.
			doItContext := self context doItContext ]
		ifFalse: [ 
			doItReceiver := doItContext := nil ].
		
	method := self compile: aStream for: doItReceiver in: doItContext.
	method isReturnSpecial
		ifTrue: [ self inform: 'Nothing to debug, the expression is optimized'.
			^ self ].
	method notNil
		ifTrue: [ self debug: method receiver: doItReceiver in: doItContext ]
]

{ #category : #private }
SpCodeDebugItCommand >> debug: aCompiledMethod receiver: anObject in: evalContext [
	| guineaPig suspendedContext debugSession |

	guineaPig := [ 
		aCompiledMethod
			valueWithReceiver: anObject
			arguments: (evalContext ifNil: [ #() ] ifNotNil: [ { evalContext } ]) ]
			newProcess.
	suspendedContext := guineaPig suspendedContext.
	
	debugSession := guineaPig newDebugSessionNamed: 'debug it' startedAt: suspendedContext.
	debugSession stepIntoUntil: [ :currentContext | 
		currentContext method == aCompiledMethod ].
	
	Smalltalk tools debugger openOn: debugSession withFullView: true.
	"(StDebugger on: debugSession)
		application: context application;
		openWithFullView"
		
]

{ #category : #execution }
SpCodeDebugItCommand >> execute [
	| selection |
	
	selection := self selectedTextOrLine.
	selection ifEmpty: [ ^ '' ].
	
	self debug: selection readStream	
]
