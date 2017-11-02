Extension { #name : #Object }

{ #category : #'*metacello-mc' }
Object class >> lastMetacelloVersionLoad [
	"Answer the last version loaded and the list of packages loaded for that version.
	 See MetacelloConfigTemplate."

	^nil -> 'default'
]

{ #category : #'*metacello-mc' }
Object class >> metacelloVersion: versionString loads: anArrayOrString [
	"Stash the last version loaded and the list of packages loaded for that version. The list
	 of packages will be used by the tools when doing 'Load Package Version'.
	See MetacelloConfigTemplate for example"
	
	"noop by default"
]
