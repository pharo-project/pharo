"
I model undeclared variable bindings. I am stored as a literal inside methods which reference me.

The compiler forwards bytecode generation to me for accessing the variable. 

In future I can profice logging and user warning when evaluated code accesses undeclared variables
"
Class {
	#name : 'UndeclaredVariable',
	#superclass : 'LiteralVariable',
	#category : 'Kernel-Variables',
	#package : 'Kernel',
	#tag : 'Variables'
}

{ #category : 'instance creation' }
UndeclaredVariable class >> possiblyRegisteredWithName: aString [

	"Get a registered undeclared variable if any, or a new unregistered undeclared variable"

	| varName |
	varName := aString asSymbol.

	Undeclared associationAt: varName ifPresent: [ :found |
		found class == Association ifTrue: [
			"Found undeclared var can be an association during current bootstrap process.
			So here we should convert it to real variable object otherwise the build will be broken"
			('Found an Association in Undeclared: ', varName) traceCr.
			found becomeForward: (self named: varName)].
		^found	].

	^self named: varName
]

{ #category : 'instance creation' }
UndeclaredVariable class >> registeredWithName: aString [

	| var |
	var := self possiblyRegisteredWithName: aString.
	(var isUndeclaredVariable and: [ var isRegistered not ]) ifTrue: [ var register ].
	^ var
]

{ #category : 'queries' }
UndeclaredVariable >> definingClass [
	"Nobody defines undeclared variable"
	^nil
]

{ #category : 'code generation' }
UndeclaredVariable >> emitStore: aMethodBuilder [
	| tempName |
	"Swap implementation comes from the superclass"
	tempName := '0TempForStackManipulation'.
	aMethodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushLiteral: self;
		pushTemp: tempName;
		send: #runtimeUndeclaredWrite:
]

{ #category : 'code generation' }
UndeclaredVariable >> emitValue: aMethodBuilder [
	aMethodBuilder
		pushLiteral: self;
		send: #runtimeUndeclaredRead
]

{ #category : 'registry' }
UndeclaredVariable >> isRegistered [
	^Undeclared includesKey: name
]

{ #category : 'testing' }
UndeclaredVariable >> isUndeclaredVariable [

	^ true
]

{ #category : 'registry' }
UndeclaredVariable >> register [
	Undeclared add: self
]

{ #category : 'registry' }
UndeclaredVariable >> registerFromNode: aNode [
	| methodNode |
	methodNode := aNode methodNode.
	methodNode propertyAt: #Undeclareds ifAbsentPut: [WeakSet new].
	(methodNode propertyAt: #Undeclareds) add: self.
	self register
]

{ #category : 'registry' }
UndeclaredVariable >> registerMethod: aCompiledMethod [
	self registeredMethods add: aCompiledMethod
]

{ #category : 'queries' }
UndeclaredVariable >> registeredMethods [
	"We hold weakly on all compiledMethods that use this Undeclared
	note: they might not be installed, e.g. long running method on the stack or a tool holding on to it"
	^ self propertyAt: #registeredMethods ifAbsentPut: [ WeakIdentitySet new ]
]

{ #category : 'code generation' }
UndeclaredVariable >> runtimeUndeclaredRead [

	<debuggerCompleteToSender>
	^ UndeclaredVariableRead new
		messageText: 'Attempt to read undeclared variable ' , self name;
		variable: self;
		signal
]

{ #category : 'code generation' }
UndeclaredVariable >> runtimeUndeclaredWrite: anObject [

	<debuggerCompleteToSender>
	^ UndeclaredVariableWrite new
		messageText: 'Attempt to write to undeclared variable ' , self name;
		variable: self;
		value: anObject;
		signal
]

{ #category : 'registry' }
UndeclaredVariable >> unregister [
	Undeclared removeKey: name ifAbsent: [ ]
]

{ #category : 'queries' }
UndeclaredVariable >> usingMethods [
	"using methods returns just those that are installed in a class"
	^ self registeredMethods select: [ :method | method isInstalled ]
]
