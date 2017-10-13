running
nodeCritiquesOf: aMethod
	"this method exists because we have rules that check
	ast nodes, but we display their critiques together with
	method critiques… And because we do not want to re-check
	the banned rules for exery node, I do in once for all
	the nodes of a method. I have to find a better solution to
	do this, but for thow the method will stay like this."


	| rules critiques manifest builder |
	
	builder := TheManifestBuilder new.
	manifest := builder manifestOf: aMethod.
		
	rules := (ReRuleManager managerFor: aMethod) nodeRules.
	critiques := Set new.
	rules := rules
		reject: [ :rule |
			manifest isNotNil and:
			[ aMethod banChecksForValidation
				anySatisfy: [ :banLevel |
					builder bansRule: rule for: banLevel ] ] ].
		
		aMethod ast nodesDo: [ :node |
			rules do: [ :rule |
			[
			  rule
				  check: node
				  forCritiquesDo: [ :critique |
						critique sourceAnchor initializeEnitity: aMethod.
					  critiques add: critique ]
			] on: Error
			  do: [ :er |
				ReExceptionStrategy current
					handle: er
					about: aMethod
					forProppertiesDo: [ :prop |
						critiques add: prop ] ].
			Processor yield ] ].
	
	ReSystemAnnouncer uniqueInstance notifyEntity: aMethod criticizedWith: critiques.
	
	^ critiques