"
An abstract class containing common tools for match and rewrite rules
"
Class {
	#name : 'RePatternCodeRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'Renraku-Rules-Rewrite',
	#package : 'Renraku',
	#tag : 'Rules-Rewrite'
}

{ #category : 'testing' }
RePatternCodeRule class >> isAbstract [

	 ^ self = RePatternCodeRule
]

{ #category : 'testing' }
RePatternCodeRule class >> isVisible [

	^ self name ~=  #RePatternCodeRule
]

{ #category : 'testing' }
RePatternCodeRule class >> noteCompilationOf: aSelector meta: isMeta [

	 aSelector == #initialize & isMeta not ifTrue: [
		ReRuleManager reset ]
]

{ #category : 'hooks' }
RePatternCodeRule >> afterCheck: aNode mappings: mappingDict [
	"after a node was match you can do a post-validation here on the node and the dictionalry of wildcars with matched nodes as values"

	^ true
]

{ #category : 'private' }
RePatternCodeRule >> flattenMatchMap: aDictionary [
	^ aDictionary keysAndValuesDo: [ :key :val |
		((key isKindOf: ASTProgramNode) and: [
		 key isPatternNode ])
			ifTrue: [
				aDictionary
					at: key pattern
					put: val ] ]
]
