"
I keep information about the critiques that exist on certain entities.
I'm invoked usually as follows:
	
	Point critiques
	(Point >> #degrees) critiques
	
I contain some functionality shared between code entities
"
Class {
	#name : 'ReCriticEngine',
	#superclass : 'Object',
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'Renraku-Utility',
	#package : 'Renraku',
	#tag : 'Utility'
}

{ #category : 'running' }
ReCriticEngine class >> critiquesOf: anEntity [

	^ self uniqueInstance critiquesOf: anEntity
]

{ #category : 'banning' }
ReCriticEngine class >> guidedBy: anEntity ban: aCritique [

	^ self uniqueInstance guidedBy: anEntity ban: aCritique
]

{ #category : 'running' }
ReCriticEngine class >> nodeCritiquesOf: anEntity [

	^ self uniqueInstance nodeCritiquesOf: anEntity
]

{ #category : 'accessing' }
ReCriticEngine class >> uniqueInstance [

	^ uniqueInstance ifNil: [ uniqueInstance := self new ]
]

{ #category : 'running' }
ReCriticEngine >> critiquesOf: anEntity [
	| rules critiques manifest builder |

	builder := TheManifestBuilder new.
	manifest := builder manifestOf: anEntity.

	rules := anEntity suitableRulesFrom: (ReRuleManager managerFor: anEntity).
	critiques := Set new.
	rules
		reject: [ :rule |
			manifest isNotNil and:
			[ anEntity banChecksForValidation
				anySatisfy: [ :banLevel |
					builder bansRule: rule for: banLevel ] ] ]
		thenDo: [ :rule |
			[ rule resetResult.
			  rule
				  check: anEntity
				  forCritiquesDo: [ :critic |
					  critiques add: critic ]
			] on: Error
			  do: [ :er |
				ReExceptionStrategy current
					handle: er
					about: anEntity
					forPropertiesDo: [ :prop |
						critiques add: prop ] ].
			Processor yield ].

	ReSystemAnnouncer uniqueInstance notifyEntity: anEntity criticizedWith: critiques.

	^ critiques
]

{ #category : 'banning' }
ReCriticEngine >> guidedBy: anEntity ban: aCritique [

	| candidates presenter |
	candidates := anEntity banLevelCandidates.
	presenter := SpSelectDialog new.
	(presenter
		title: 'On which level do you want to ban the rule?';
		items: candidates;
		display: [ :each | each reBanName ];
		onAccept: [ :dialog |
			dialog presenter inform: dialog presenter selectedItem asString ];
		openModal ) ifNotNil: [
		:entity | entity ban: aCritique ]
]

{ #category : 'running' }
ReCriticEngine >> nodeCritiquesOf: aMethod [
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
	rules := rules reject: [ :rule | manifest isNotNil and: [ aMethod banChecksForValidation anySatisfy: [ :banLevel | builder bansRule: rule for: banLevel ] ] ].
	rules do: [ :rule |
		(rule shouldCheckMethod: aMethod) ifTrue: [
			| ast |
			ast := aMethod ast.
			"for rewrite rules, we run every rule on a copy of the ast"
			rule isRewriteRule ifTrue: [ ast := ast copy ].
			ast nodesDo: [ :node |
				[
				rule check: node forCritiquesDo: [ :critique |
					critique sourceAnchor initializeEnitity: aMethod.
					critiques add: critique ] ]
					on: Error
					do: [ :er | ReExceptionStrategy current handle: er about: aMethod forPropertiesDo: [ :prop | critiques add: prop ] ].
				Processor yield ] ] ].

	ReSystemAnnouncer uniqueInstance notifyEntity: aMethod criticizedWith: critiques.

	^ critiques
]
