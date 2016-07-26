accessing
mappingFor: aNode
	| method |
	method := self methodNode.
	method ifNil: [^aNode].
	^method mappingFor: aNode