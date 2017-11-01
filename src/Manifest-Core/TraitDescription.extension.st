Extension { #name : #TraitDescription }

{ #category : #'*Manifest-Core' }
TraitDescription >> criticClass [
	"Return the class of the receiver for the critic browser. This behavior may be folded later by changing the name of this method or using another one."
	
	^ self
]

{ #category : #'*Manifest-Core' }
TraitDescription >> criticNameOn: aStream [
	"This behavior may be folded later by changing the name of this method or using another one."
	
	aStream 
		<< self name 
		<< ' (' 
		<< self category 
		<< ')' 
]

{ #category : #'*Manifest-Core' }
TraitDescription >> manifestBuilderForRuleChecker: aRuleChecker [
	"Return the manifestsince the rulechecker is keeping a cache, we ask it back"

	^ aRuleChecker manifestBuilderOfClass: self
]

{ #category : #'*Manifest-Core' }
TraitDescription >> mcWorkingCopy [
	MCWorkingCopy managersForClass:  self do: [: package | ^ package ]
]
