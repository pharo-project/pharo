updating
selectNodePath: anArray
	"select a node from a path based on wrapper node equivalence"
	
	| found |
	anArray isNil ifTrue: [^ self].
	anArray isEmpty ifTrue: [^ self].
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents = anArray first
							or: [anArray first isNil])
						ifTrue: [found := each]]].
	found
		ifNotNil: [
			anArray size = 1
				ifTrue: [^ container listManager addToSelection: found].
			found firstChild
				ifNotNil: [:fc | fc selectNodePath: anArray allButFirst]].
