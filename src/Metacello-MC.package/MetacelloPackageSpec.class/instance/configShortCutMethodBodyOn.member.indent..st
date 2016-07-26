printing
configShortCutMethodBodyOn: aStream member: aMember indent: indent

	| hasFile hasRepositories hasPreLoadDoIt hasPostLoadDoIt hasRequiresOrIncludesOrAnswers |
	hasFile := file ~~ nil.
	hasRepositories := self repositorySpecs size > 0.
	hasPreLoadDoIt := self getPreLoadDoIt ~~ nil.
	hasPostLoadDoIt := self getPostLoadDoIt ~~ nil.
	hasRequiresOrIncludesOrAnswers := (self requires isEmpty and: [ self includes isEmpty and: [self answers isEmpty ]]) not.
	hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
		ifTrue: [
			aStream 
				nextPutAll: 'package: ', self name printString, ' ';
				nextPutAll: aMember methodUpdateSelector asString, ' ['; cr.
			aStream tab: indent + 1; nextPutAll: 'spec '.
			self configMethodBodyOn: aStream hasName: false indent: indent + 2.
			aStream nextPutAll: ' ]'.
			^self ].
	aStream nextPutAll: 'package: ', self name printString.
	hasFile
		ifTrue: [ aStream nextPutAll: ' with: ', file printString ]