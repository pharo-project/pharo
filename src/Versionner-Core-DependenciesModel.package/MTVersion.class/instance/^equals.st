comparing
= other
	(self name = other name) ifFalse: [ ^false ].
	(self author = other author) ifFalse: [ ^false ].
	(self versionString = other versionString) ifFalse: [ ^false ].
	(self description = other description) ifFalse: [ ^false ].
	(self timestamp = other timestamp) ifFalse: [ ^false ].
	(self project = other project) ifFalse: [ ^false ].
	^true.