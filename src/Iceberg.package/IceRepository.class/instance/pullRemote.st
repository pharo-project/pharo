accessing remotes
pullRemote
	^ pullRemote ifNil: [ pullRemote := self pushRemote ]