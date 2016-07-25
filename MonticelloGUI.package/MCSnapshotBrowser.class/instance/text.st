text
text
	self switchIsComment ifTrue: [ ^ self classCommentString ].
	methodSelection ifNotNil: [ ^ methodSelection source ].
	protocolSelection ifNotNil: [ ^ '' ].
	classSelection ifNotNil: [
		^ self switchIsClass 
			ifTrue: [ self metaclassDefinitionString ]
			ifFalse: [ self classDefinitionString ] ].
	categorySelection ifNil: [ ^ self scriptDefinitionString ].
	^ ''