instance creation
on: aStream globalEnvironment: aDictionary do: aBlock
	
	| anEncoder |
	anEncoder := self on: aStream globalEnvironment: aDictionary.
	^ [ aBlock value: anEncoder ] ensure: [ anEncoder flush ]