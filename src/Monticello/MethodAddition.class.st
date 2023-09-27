"
I represent the addition of a method to a class.  I can produce the CompiledMethod, install it, and then notify the system that the method has been added.  This allows Monticello to implement atomic addition.  A loader can compile all classes and methods first and then install all methods only after they have been all compiled, and in a way that executes little code.
"
Class {
	#name : 'MethodAddition',
	#superclass : 'Object',
	#instVars : [
		'text',
		'changeStamp',
		'logSource',
		'myClass',
		'selector',
		'compiledMethod',
		'priorMethod',
		'protocolName',
		'priorProtocol',
		'priorOrigin'
	],
	#category : 'Monticello-Loading',
	#package : 'Monticello',
	#tag : 'Loading'
}

{ #category : 'compilation' }
MethodAddition >> compile [
	"This method is the how compiling a method used to work.  All these steps were done at once.
     This method should not normally be used, because the whole point of MethodAddition is to let
	you first create a compiled method and then install the method later."

	self
		createCompiledMethod;
		installMethod;
		notifyObservers.
	^ selector
]

{ #category : 'compilation' }
MethodAddition >> compile: aString classified: aProtocolName withStamp: aString2 logSource: aBoolean inClass: aClass [

	text := aString.
	protocolName := aProtocolName.
	changeStamp := aString2.
	logSource := aBoolean.
	myClass := aClass
]

{ #category : 'operations' }
MethodAddition >> createCompiledMethod [
	"CyrilFerlicot: Why do we need to explicitly write to the source? Why do we call the compiler?
	
	Can't we just call #compile:classfied: here?"
	compiledMethod := myClass compiler permitUndeclared: true; compile: text asString.
	selector := compiledMethod selector.
	self writeSourceToLog.
	priorMethod := myClass compiledMethodAt: selector ifAbsent: [ nil ].
	priorOrigin := compiledMethod origin.
	priorProtocol := myClass protocolOfSelector: selector
]

{ #category : 'operations' }
MethodAddition >> installMethod [
	SystemAnnouncer uniqueInstance
		suspendAllWhile: [ myClass addSelector: selector withMethod: compiledMethod ]
]

{ #category : 'notifying' }
MethodAddition >> notifyObservers [

	SystemAnnouncer uniqueInstance suspendAllWhile: [ myClass classify: selector under: protocolName ].
	(priorMethod isNil or: [ priorOrigin ~= compiledMethod origin ])
		ifTrue: [ SystemAnnouncer uniqueInstance methodAdded: compiledMethod configuredWith: [ :event | event propertyAt: #eventSource put: #Monticello ] ]
		ifFalse: [
			SystemAnnouncer uniqueInstance methodChangedFrom: priorMethod to: compiledMethod oldProtocol: priorProtocol.
			priorProtocol name = protocolName ifFalse: [ SystemAnnouncer uniqueInstance methodRecategorized: compiledMethod oldProtocol: priorProtocol ] ].
	"The following code doesn't seem to do anything."
	myClass instanceSide noteCompilationOf: compiledMethod meta: myClass isClassSide
]

{ #category : 'accessing' }
MethodAddition >> priorProtocol [
	^ priorProtocol
]

{ #category : 'operations' }
MethodAddition >> writeSourceToLog [

	logSource ifTrue: [
		myClass
			logMethodSource: text
			forMethod: compiledMethod
			inProtocol: (myClass ensureProtocol: protocolName)
			withStamp: changeStamp ]
]
