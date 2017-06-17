lookup
for: aReturnCode
	^ (self allSubclasses reject: #isAbstract)
		detect: [ :class | class canHandle: aReturnCode ]
		ifNone: [ LGitNoReturnCodeHandlerDefined signal ]