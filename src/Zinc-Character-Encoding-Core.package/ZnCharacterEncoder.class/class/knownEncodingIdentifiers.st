accessing
knownEncodingIdentifiers
	"Return a collection of all known encoding identifiers in the system"
	
	self = ZnCharacterEncoder ifFalse: [ ^ #() ].
	^ Array streamContents: [ :all |
		self allSubclassesDo: [ :subClass |
			all nextPutAll: subClass knownEncodingIdentifiers ] ]