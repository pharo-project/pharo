temps
bindTemp: name
	"Declare a temporary; error not if a field or class variable or out-of-scope temp.
	 Read the comment in Encoder5>>bindBlockArg:within: and subclass implementations."
	self supportsClosureOpcodes ifFalse:
		[^super bindTemp: name].
	scopeTable at: name ifPresent:
		[:node|
			(requestor interactive)
						ifTrue: [self evaluateShadowingInteractivelyOf: node]
						ifFalse: [self evaluateShadowingNotInteractivelyOf: node] ].
	^self reallyBind: name