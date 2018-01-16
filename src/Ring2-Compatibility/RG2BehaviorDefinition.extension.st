Extension { #name : #RG2BehaviorDefinition }

{ #category : #'*Ring2-Compatibility' }
RG2BehaviorDefinition >> realClass [

	^ self class environment classOrTraitNamed: self name
]
