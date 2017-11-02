Extension { #name : #MetacelloProjectReferenceSpec }

{ #category : #'*Versionner-Commit' }
MetacelloProjectReferenceSpec >> fixedUpdateForSpawnMethod: sourceSpec [
	"This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"

	| versionString |
	
	"This is clearly a hack to obtain correct 'fixed' version number"
	self projectReference className ifNotNil: [ :className |
		 className asClassIfPresent: [ :class |
			versionString := (class project version: sourceSpec versionString) versionString ] ].
	self updateForSpawnMethod: sourceSpec.
	self versionString: versionString.
]
