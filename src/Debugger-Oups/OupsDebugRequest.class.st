"
The information required to open a debugger can take multiple forms.
This object is a builder, to be filled with the relevant information by any object that wants to open a debugger.
Once this object is filled, call its #debug or #warning method to open a debugger/send a warning.

# Information That Can Be Provided (and their type)
- exception: Exception
- context: Context
- label: String
- process: Process
- compiledMethod: CompiledMethod

# Notes
- You NEED to provide an exception OR a context. The other information are optional.
- If an exception and a label are provided, the label will be ignored (in favour of the #description of the exception)
- Providing a compiledMethod will make it so the execution will be stepped until it is in that method , prior to the debugger opening

# Instance Variables
- exception: Exception.
- context: Context.
- label: String. If not provided, the result of ""self defaultLabel_"" will be used.
- process: Process. If not provided, the result of ""self defaultProcess"" will be used.
- compiledMethod: CompiledMethod
- preDebugActions: OrderedCollection<BlockClosure<DebugSession, nil>>. A list of blocks, taking as arguments a DebugSession on the execution to be debugged. These will be executed before the debugger opens.

# Technical Details
- the setters are meant to be used by the caller, to fill this object with information
- the getters are meant to be used by whatever objects is opening a debugger based on the information contained in this object
"
Class {
	#name : #OupsDebugRequest,
	#superclass : #Object,
	#instVars : [
		'exception',
		'context',
		'label',
		'process',
		'compiledMethod',
		'preDebugActions',
		'debugSession'
	],
	#category : #'Debugger-Oups-infrastructure'
}

{ #category : #'instance creation' }
OupsDebugRequest class >> new [
	self shouldNotImplement
]

{ #category : #'instance creation' }
OupsDebugRequest class >> newForContext: aContext [
	<debuggerCompleteToSender>
	^ self basicNew context: aContext; yourself
]

{ #category : #'instance creation' }
OupsDebugRequest class >> newForException: anException [
	<debuggerCompleteToSender>
	^ self basicNew exception: anException; yourself
]

{ #category : #accessors }
OupsDebugRequest >> compiledMethod: anObject [
	compiledMethod := anObject
]

{ #category : #preDebugActions }
OupsDebugRequest >> computePreDebugActions [
	"Fill the preDebugActions variable."
	"If compiledMethod is not nil, this DebugRequest has been created to debug the execution of a specific method. Add a pre-debug action to step the execution until that method is reached"
	compiledMethod ifNotNil: [ self preDebugActions add: (self preDebugActionForDebugItMethod: compiledMethod) ]
]

{ #category : #accessors }
OupsDebugRequest >> context: anObject [
	context := anObject
]

{ #category : #accessors }
OupsDebugRequest >> debugSession [

	"If a DebugSession is requested for this debug request, ask the process to create one. Subsequent calls to this method will return the same DebugSession (and not create another one)"

	debugSession ifNil: [ 
		debugSession := (self process
			                 newDebugSessionNamed: self exception description
			                 startedAt: self exception signalerContext)
			                exception: self exception;
			                yourself ].
	^ debugSession
]

{ #category : #'default values' }
OupsDebugRequest >> defaultLabel [
	^ 'No label was provided'
]

{ #category : #'default values' }
OupsDebugRequest >> defaultProcess [
	^ Processor activeProcess
]

{ #category : #'default values' }
OupsDebugRequest >> defaultUIManager [
	^ UIManager default
]

{ #category : #accessors }
OupsDebugRequest >> exception [
	"If an exception was provided, return it, otherwise create a dummy exception using the provided context and label (if no label was provided, use the result of 'self defaultLabel_' "
	^ exception ifNil: [ self nullExceptionFor: context named: (label ifNil: [ self defaultLabel ]) ]
]

{ #category : #accessors }
OupsDebugRequest >> exception: anObject [
	exception := anObject
]

{ #category : #accessors }
OupsDebugRequest >> label: anObject [
	label := anObject
]

{ #category : #helper }
OupsDebugRequest >> nullExceptionFor: aContext named: aString [
	^ (OupsNullException fromSignallerContext: aContext)
		messageText: aString;
		yourself
]

{ #category : #preDebugActions }
OupsDebugRequest >> preDebugActionForDebugItMethod: aCompiledMethod [
	"Return a PreDebug action that will step the debugged execution until it is in a call to @aCompiledMethod"

	^ [ :session | 
	session
		stepIntoUntil: [ :currentContext | currentContext method == aCompiledMethod ] ]
]

{ #category : #accessors }
OupsDebugRequest >> preDebugActions [
	preDebugActions ifNil: [ preDebugActions := OrderedCollection new ].
	^ preDebugActions
]

{ #category : #accessors }
OupsDebugRequest >> process [
	"If a process was provided, return it, otherwise use the result of 'self defaultProcess'"
	^ process ifNil: [ self defaultProcess ]
]

{ #category : #accessors }
OupsDebugRequest >> process: anObject [
	process := anObject
]

{ #category : #submit }
OupsDebugRequest >> submit [
	<debuggerCompleteToSender>
	"Submit this debug request to the system. The system will typically open a debugger for it"
	OupsDebuggerSystem new handleDebugRequest: self
]
