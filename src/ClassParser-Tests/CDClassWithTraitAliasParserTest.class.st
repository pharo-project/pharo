Class {
	#name : #CDClassWithTraitAliasParserTest,
	#superclass : #CDTraitCompositionClassParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDClassWithTraitAliasParserTest >> aliasName [
	
	^ #alias
]

{ #category : #accessing }
CDClassWithTraitAliasParserTest >> existingSelectorName [
	
	^ #existing
]

{ #category : #tests }
CDClassWithTraitAliasParserTest >> testTraitAliasShouldBeFromAliasName [

	self assert: (self traitCompositionNode aliases keys includes: self aliasName)
]

{ #category : #tests }
CDClassWithTraitAliasParserTest >> testTraitAliasTargetSelectorShouldBeExistingSelector [

	self assert: (self traitCompositionNode aliases values includes: self existingSelectorName)
]

{ #category : #tests }
CDClassWithTraitAliasParserTest >> testTraitDefinitionNodeIsTraitAliasNode [

	self assert: self traitCompositionNode isTraitAliasNode
]

{ #category : #helpers }
CDClassWithTraitAliasParserTest >> traitDefinition [

	^ 'MyTrait @ \{#''{aliasName}'' -> #''{existingSelectorName}''\}'
		format: { 
			'aliasName' -> self aliasName.
			'existingSelectorName' -> self existingSelectorName. } asDictionary
]
