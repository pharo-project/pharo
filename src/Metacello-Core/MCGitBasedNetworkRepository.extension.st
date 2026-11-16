Extension { #name : 'MCGitBasedNetworkRepository' }

{ #category : '*Metacello-Core' }
MCGitBasedNetworkRepository >> baselineOf: aProjectName [
	"The baseline at the ref my description names"

	^ self baselineOf: aProjectName atVersion: projectVersion
]
