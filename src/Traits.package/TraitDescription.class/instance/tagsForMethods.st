accessing tags
tagsForMethods
	"Any method could be tagged with multiple symbols for user purpose. 
	And class contains all method tags which used or not used yet by methods.
	For now we could only implemented it on top of Protocol. 
	It supposed to not include any method tags inherited from Traits 
	which is opposite to current Protocol implementation.
	And extension protocol is not treated as tag"
	| allProtocols tags |
	allProtocols := self organization protocols 
		reject: [ :each | each name = Protocol unclassified | each isExtensionProtocol ].
		
	tags := OrderedCollection new.
	allProtocols do: [ :each | 
		(self isLocalMethodsProtocol: each) ifTrue: [ tags add: each name ]].

	^tags