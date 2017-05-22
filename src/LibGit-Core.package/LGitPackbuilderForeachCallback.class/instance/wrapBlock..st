initialization
wrapBlock: aBlock

	^[ :buffer :length :payload |
		aBlock 
			value: buffer 
			value: length
			value: (LGitPackbuilderForeachPayload fromHandle: payload) ]