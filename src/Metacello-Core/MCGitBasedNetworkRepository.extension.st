Extension { #name : 'MCGitBasedNetworkRepository' }

{ #category : '*Metacello-Core' }
MCGitBasedNetworkRepository >> baselineOf: aProjectName [
	"The baseline at the ref my description names"

	^ self baselineOf: aProjectName atVersion: projectVersion
]

{ #category : '*Metacello-Core' }
MCGitBasedNetworkRepository >> projectUrl [
	"My description without the version and the path it names: type://owner/name"

	^ self class basicDescription , '://' , self projectPath
]
