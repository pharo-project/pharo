method accessing
protocolsFor: aSelector 
	| change |
	change := self isMeta 
				ifTrue: [model changes changeForMetaclass: name selector: aSelector]
				ifFalse: [model changes changeForClass: name selector: aSelector].
	^change isNil 
		ifTrue: 
			[self isDefined 
				ifTrue: 
					[Array with: (RBBrowserEnvironment new whichProtocolIncludes: aSelector in: self realClass)]
				ifFalse: [#(#accessing)]]
		ifFalse: [change protocols]