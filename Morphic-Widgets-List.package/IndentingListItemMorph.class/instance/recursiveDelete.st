private-container protocol
recursiveDelete

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode recursiveDelete].
	].
	self delete
	