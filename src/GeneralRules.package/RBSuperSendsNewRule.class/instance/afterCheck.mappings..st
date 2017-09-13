hooks
afterCheck: aNode mappings: mappingDict
	^ aNode methodNode
		ifNil: [ false ]
		ifNotNil: [ :methodNode |
			methodNode method
				ifNil: [ false ]
				ifNotNil: [ :method |
					method methodClass
						ifNil: [ false ]
						ifNotNil: #isMeta ] ]