Extension { #name : 'OSPlatform' }

{ #category : '*Files' }
OSPlatform >> createStdioFileFor: moniker [
	"This is a hack due to Windows vm limitations."

	self error: moniker , ' is unavailable'
]
