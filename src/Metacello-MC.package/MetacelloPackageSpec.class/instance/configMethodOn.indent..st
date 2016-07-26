printing
configMethodOn: aStream indent: indent

	| hasRepositories hasPreLoadDoIt hasPostLoadDoIt hasRequiresOrIncludesOrAnswers hasFile |
	hasFile := file ~~ nil.
	hasRepositories := self repositorySpecs size > 0.
	hasPreLoadDoIt := self getPreLoadDoIt ~~ nil.
	hasPostLoadDoIt := self getPostLoadDoIt ~~ nil.
	hasRequiresOrIncludesOrAnswers := (self requires isEmpty and: [ self includes isEmpty and: [self answers isEmpty ]]) not.
	aStream tab: indent; nextPutAll: 'spec '.
	hasFile | hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
		ifTrue: [
			aStream cr; tab: indent + 1; nextPutAll: 'name: ', self name printString; nextPut: $;.
			self configMethodBodyOn: aStream hasName: true indent: indent + 1 ]
		ifFalse: [
			aStream nextPutAll: 'name: ', self name printString ]