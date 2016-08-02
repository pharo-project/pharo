*renraku
critiques
	<eProperty>
	| rules critiques manifest builder |
	
	builder  := TheManifestBuilder new.
	manifest := self compilationContext
		ifNil: [ nil ]
		ifNotNil: [ :context |
	 		context getClass
				ifNil: [ nil ]
				ifNotNil: [ :class |
					class
						compiledMethodAt: self selector
						ifPresent: [ :method | builder manifestOf: method ]
						ifAbsent: [ nil ] ] ].
		
	rules    := ReRuleManager uniqueInstance nodeRules.
	critiques  := Set new.

	rules := rules
		reject: [ :rule |
			manifest isNotNil and:
			[ (builder bansRule: rule for: self method) or:
			[  builder bansRule: rule for: self methodClass ] ] ].

	

	self nodesDo: [ :node |
			rules do: [ :rule |
				[
					rule
						check: node
				 		forCritiquesDo: [ :critique |
							critique sourceAnchor initializeEnitity: self.
					  		critiques add: critique ]
				] on: Error
			  	  do: [ :er | ReSettings showErrors ifTrue: [ er pass ] ] ].
			Processor yield ].
	
	ReSystemAnnouncer uniqueInstance notifyEntity: self criticizedWith: critiques. 
	
		^ critiques