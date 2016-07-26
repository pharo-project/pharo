*GT-Debugger
styleContext: aContext from: aDebugger.

	(aDebugger selectedContext == aContext) ifTrue: [ ^ {TextColor color: Smalltalk ui theme textColor}  ].

	(aContext receiver class = aDebugger selectedContext receiver class)
		ifTrue: [ ^ self sameClassContextStyleFor: aContext ].
	(aContext receiver class package = aDebugger selectedContext receiver class package)
		ifTrue: [ ^ self samePackageContextStyleFor: aContext ]. 

	^ self unrelatedContextStyleFor: aContext