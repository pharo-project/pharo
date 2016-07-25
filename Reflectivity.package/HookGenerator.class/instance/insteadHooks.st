results
insteadHooks
	| insteadLinks |
	insteadLinks := ((links select: [ :each | each control = #instead ])) asArray.
	insteadLinks isEmpty ifTrue: [ ^#()].
	^(self hookFor: insteadLinks last) parent: node 