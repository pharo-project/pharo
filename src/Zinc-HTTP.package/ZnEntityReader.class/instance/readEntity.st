public
readEntity
	| entity |
	self canReadContent ifFalse: [ ^ nil ].
	entity := self readEntityFromStream.
	^ entity isEmpty
		ifTrue: [ nil ]
		ifFalse: [ entity ]    