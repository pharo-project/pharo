temps
bindTemp: name 
	"Declare a temporary; error not if a field or class variable."
	scopeTable at: name ifPresent:
	[:node|
			(requestor interactive)
						ifTrue: [self evaluateShadowingInteractivelyOf: node]
						ifFalse: [self evaluateShadowingNotInteractivelyOf: node] ].
	^self reallyBind: name