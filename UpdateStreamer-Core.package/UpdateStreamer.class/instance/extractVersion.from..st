private - actions
extractVersion: aVersion from: updateList
	"Pull out the part of the list that applies to a version."
	"self new extractVersion: 'Pharo1.1' from: ..."
	
	| listContents versIndex |
	self flag: #shouldUseADictionary.
	listContents := self groupUpdatesByVersion: updateList.
	versIndex := (listContents collect: [ :pair | pair first]) indexOf: aVersion.
	versIndex = 0 ifTrue: [^ Array new].
	^ (listContents at: versIndex) last