*Reflectivity
invalidate
	| reflectiveMethod |
	self reflectivityDisabled ifTrue: [ ^self ].
	
	reflectiveMethod := self reflectiveMethod.
	reflectiveMethod ifNil: [^self "do nothing"].
	(self isRealPrimitive or: (reflectiveMethod ast metaLinkOptionsFromClassAndMethod includes: #optionCompileOnLinkInstallation))
					ifTrue: [reflectiveMethod compileAndInstallCompiledMethod ] 
					ifFalse: [reflectiveMethod installReflectiveMethod]
	