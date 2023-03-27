Extension { #name : #ClassDescription }

{ #category : #'*Deprecated12' }
ClassDescription >> allProtocolsUpTo: mostGenericClass [
	"Answer a list of all the method protocols of the receiver and all its superclasses, up through mostGenericClass"

	| protocols |
	self deprecated: 'This methods will be removed in the next version of Pharo. If you use it, you can inline it.'.
	protocols := (self allSuperclassesIncluding: mostGenericClass) flatCollectAsSet: [ :aClass | aClass organization protocolNames ].
	protocols remove: Protocol nullProtocolName ifAbsent: [  ].
	^ protocols
]
