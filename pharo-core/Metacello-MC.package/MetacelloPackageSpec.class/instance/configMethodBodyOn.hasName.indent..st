printing
configMethodBodyOn: aStream hasName: hasName indent: indent

	| hasFile hasRepositories hasPreLoadDoIt hasPostLoadDoIt hasRequiresOrIncludesOrAnswers |
	hasFile := file ~~ nil.
	hasRepositories := self repositorySpecs size > 0.
	hasPreLoadDoIt := self getPreLoadDoIt ~~ nil.
	hasPostLoadDoIt := self getPostLoadDoIt ~~ nil.
	hasRequiresOrIncludesOrAnswers := (self requires isEmpty and: [ self includes isEmpty and: [self answers isEmpty ]]) not.
	hasRequiresOrIncludesOrAnswers
		ifTrue: [ 
			self 
				configMethodBodyOn: aStream 
				hasName: hasName 
				cascading: hasFile | hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt
				indent: indent ].
	self 
		configMethodOn: aStream 
		for: file 
		selector: 'file: ' 
		cascading: hasName | hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers 
		cascade: hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt 
		indent: indent.
	hasRepositories
		ifTrue: [ 
			(self repositorySpecs size > 1)
				ifTrue: [ 
					hasName | hasFile | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
						ifTrue: [ 
							aStream cr;
							tab: indent. ].
					aStream 
						nextPutAll: 'repositories: ['; 
						cr;
						tab: indent + 1;
						nextPutAll: 'spec'; 
						cr.
					self repositories configMethodCascadeOn: aStream indent: indent + 1.
					aStream nextPutAll: ' ]' ]
				ifFalse: [ 
					hasName | hasFile | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
						ifTrue: [ aStream cr; tab: indent ].
					self repositories configMethodCascadeOn: aStream indent: indent ].
			hasPreLoadDoIt | hasPostLoadDoIt ifTrue: [ aStream nextPut: $; ] ].
	self 
		configMethodOn: aStream 
		for: self getPreLoadDoIt 
		selector: 'preLoadDoIt: '
		cascading: hasName | hasFile | hasRepositories | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers 
		cascade: hasPostLoadDoIt 
		indent: indent.
	self 
		configMethodOn: aStream 
		for: self getPostLoadDoIt 
		selector: 'postLoadDoIt: ' 
		cascading: hasName | hasFile | hasRepositories | hasPreLoadDoIt | hasRequiresOrIncludesOrAnswers 
		cascade: false 
		indent: indent.
	aStream nextPut: $.