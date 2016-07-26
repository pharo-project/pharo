private
nextToken
	^nextToken ifNil: [nextToken := scanner next] ifNotNil: [nextToken]