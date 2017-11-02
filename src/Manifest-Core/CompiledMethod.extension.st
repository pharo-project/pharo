Extension { #name : #CompiledMethod }

{ #category : #'*Manifest-Core' }
CompiledMethod >> criticClass [
	"Return the class of the receiver for the critic browser. This behavior may be folded later by changing the name of this method or using another one."
	
	^ self methodClass
]

{ #category : #'*Manifest-Core' }
CompiledMethod >> criticNameOn: aStream [
	"This behavior may be folded later by changing the name of this method or using another one."
	
	aStream 
		<< self methodClass name 
		<< '>>#' 
		<< self selector  
		<< ' (' 
		<< self methodClass theNonMetaClass category 
		<< ')'
]

{ #category : #'*Manifest-Core' }
CompiledMethod >> criticTheNonMetaclassClass [
	"Return the class of the receiver for the critic browser. This behavior may be folded later by changing the name of this method or using another one."
	
	^ self methodClass theNonMetaClass
]

{ #category : #'*Manifest-Core' }
CompiledMethod >> manifestBuilderForRuleChecker: aRuleChecker [
	"Return the manifestsince the rulechecker is keeping a cache, we ask it back"

	^ aRuleChecker manifestBuilderOfMethod: self
]

{ #category : #'*Manifest-Core' }
CompiledMethod >> mcWorkingCopy [
	 MCWorkingCopy managersForClass: self methodClass selector: self selector do: [ :package | ^ package ]
]
