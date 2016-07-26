private
removeAllMethodsInProtocol: protocolName
	| protocol |
	
	protocol := self organization protocolNamed: protocolName.
	protocol ifNil: [ ^ self ].
	
	protocol methods do: [ :each | self removeSelector: each ]