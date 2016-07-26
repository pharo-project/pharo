private
"protocol: private"
doPrimitive: primitiveIndex method: meth receiver: aReceiver args: arguments 
	"Simulate a primitive method whose index is primitiveIndex.  The simulated receiver and
	 arguments are given as arguments to this message. If successful, push result and return
	 resuming context, else ^ {errCode, PrimitiveFailToken}. Any primitive which provokes
	 execution needs to be intercepted and simulated to avoid execution running away."

	| value |
	"Judicious use of primitive 19 (a null primitive that doesn't do anything) prevents
	 the debugger from entering various run-away activities such as spawning a new
	 process, etc.  Injudicious use results in the debugger not being able to debug
	 interesting code, such as the debugger itself.  hence use primitive 19 with care :-)"
	"SystemNavigation new browseAllSelect: [:m| m primitive = 19]"
	"primitiveIndex = 19 ifTrue:	[ | debugSession | 
			debugSession := Processor activeProcess newDebugSessionNamed: 'Code simulation error' startedAt: self.
			Smalltalk tools debugger openOn: debugSession withFullView: true]."

	((primitiveIndex between: 201 and: 222)
	 and: [(self objectClass: aReceiver) includesBehavior: BlockClosure]) ifTrue:
		[((primitiveIndex between: 201 and: 205)			 "BlockClosure>>value[:value:...]"
		  or: [primitiveIndex between: 221 and: 222]) ifTrue: "BlockClosure>>valueNoContextSwitch[:]"
			[^aReceiver simulateValueWithArguments: arguments caller: self].
		 primitiveIndex = 206 ifTrue:						"BlockClosure>>valueWithArguments:"
			[^aReceiver simulateValueWithArguments: arguments first caller: self]].

	primitiveIndex = 83 ifTrue: "afr 9/11/1998 19:50" "Object>>perform:[with:...]"
		[^self send: arguments first to: aReceiver with: arguments allButFirst super: false].
	primitiveIndex = 84 ifTrue: "afr 9/11/1998 19:50 & eem 8/18/2009 17:04" "Object>>perform:withArguments:"
		[^self send: arguments first to: aReceiver with: (arguments at: 2) lookupIn: (self objectClass: aReceiver)].
	primitiveIndex = 100 ifTrue: "eem 8/18/2009 16:57" "Object>>perform:withArguments:inSuperclass:"
		[arguments size = 3 
			ifTrue: [ ^self send: arguments first to: aReceiver with: (arguments at: 2) lookupIn: (arguments at: 3)]
			ifFalse: [ ^self send: (arguments at: 2) to: (arguments at: 1) with: (arguments at: 3) lookupIn: (arguments at: 4) ]
		].


	"Mutex>>primitiveEnterCriticalSection
	 Mutex>>primitiveTestAndSetOwnershipOfCriticalSection"
	(primitiveIndex = 186 or: [primitiveIndex = 187]) ifTrue:
		[| active effective |
		 active := Processor activeProcess.
		 effective := active effectiveProcess.
		 "active == effective"
		 value := primitiveIndex = 186
					ifTrue: [aReceiver primitiveEnterCriticalSectionOnBehalfOf: effective]
					ifFalse: [aReceiver primitiveTestAndSetOwnershipOfCriticalSectionOnBehalfOf: effective].
		 ^(self isPrimFailToken: value)
			ifTrue: [value]
			ifFalse: [self push: value]].

	primitiveIndex = 188 ifTrue: "eem 5/27/2008 11:10 Object>>withArgs:executeMethod:"
		[arguments size = 2 
			ifTrue: "normal primitive"
				[^Context
					sender: self
					receiver: aReceiver
					method: (arguments at: 2)
					arguments: (arguments at: 1)]
			ifFalse: "mirror primitive"
				[^Context
					sender: self
					receiver: (arguments at: 1)
					method: (arguments at: 3)
					arguments: (arguments at: 2)]
		].
	"Closure primitives"
	(primitiveIndex = 200 and: [self == aReceiver]) ifTrue:
		"ContextPart>>closureCopy:copiedValues:; simulated to get startpc right"
		[^self push: (BlockClosure
						outerContext: aReceiver
						startpc: pc + 2
						numArgs: arguments first
						copiedValues: arguments last)].

	primitiveIndex = 118 ifTrue: "tryPrimitive:withArgs:; avoid recursing in the VM"
		[(arguments size = 2
		 and: [arguments first isInteger
		 and: [(self objectClass: arguments last) == Array]]) ifFalse:
			[^Context primitiveFailTokenFor: nil].
		 ^self doPrimitive: arguments first method: meth receiver: aReceiver args: arguments last].

	value := primitiveIndex = 120 "FFI method"
				ifTrue: [(meth literalAt: 1) tryInvokeWithArguments: arguments]
				ifFalse:
					[primitiveIndex = 117 "named primitives"
						ifTrue: [self tryNamedPrimitiveIn: meth for: aReceiver withArgs: arguments]
						ifFalse: [aReceiver tryPrimitive: primitiveIndex withArgs: arguments]].

	^(self isPrimFailToken: value)
		ifTrue: [value]
		ifFalse: [self push: value]