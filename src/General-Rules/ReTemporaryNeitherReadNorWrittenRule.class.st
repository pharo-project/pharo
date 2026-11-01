"
There is something wrong in the method as you have variables that are either nor read or not written (or both).
"
Class {
	#name : 'ReTemporaryNeitherReadNorWrittenRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReTemporaryNeitherReadNorWrittenRule class >> groupSelector [

	^ #optimizationGroup
]

{ #category : 'accessing' }
ReTemporaryNeitherReadNorWrittenRule class >> ruleName [
	^ 'Temporary variables not read or not written'
]

{ #category : 'running' }
ReTemporaryNeitherReadNorWrittenRule >> check: aNode forCritiquesDo: aBlock [
	aNode isTempVariable ifFalse: [ ^ self ].
	aNode isDefinition ifFalse: [ ^ self ].
	aNode variable isReferenced ifFalse: [
		aBlock cull: (self critiqueFor: aNode) ]
]

{ #category : 'helpers' }
ReTemporaryNeitherReadNorWrittenRule >> critiqueFor: aNode [
	"Answer a critique whose change removes the unused temporary variable defined by aNode.
	The change needs an installed method to transform: when the node does not belong to one
	(e.g. code that is not compiled into its class yet), only report the problem."

	| methodNode methodClass |
	methodNode := aNode methodNode.
	methodClass := methodNode ifNotNil: [ methodNode methodClass ].
	(methodClass isNotNil and: [ methodClass includesSelector: methodNode selector ]) ifFalse: [ ^ super critiqueFor: aNode ].

	^ (ReRefactoringCritique withAnchor: (self anchorFor: aNode) by: self)
		  tinyHint: aNode name;
		  refactoring: (RBRemoveTemporaryVariableTransformation variable: aNode name inMethod: methodNode selector inClass: methodClass);
		  yourself
]
