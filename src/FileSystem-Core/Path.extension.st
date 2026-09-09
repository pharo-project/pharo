Extension { #name : 'Path' }

{ #category : '*FileSystem-Core' }
Path >> relativeToReference: aReference [
	^ self relativeToPath: aReference path
]
