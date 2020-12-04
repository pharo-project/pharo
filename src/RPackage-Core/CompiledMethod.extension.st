Extension { #name : #CompiledMethod }

{ #category : #'*rpackage-core' }
CompiledMethod >> isDefinedInPackage: anRPackage [
	^ anRPackage includesDefinedSelector: self selector ofClass: self methodClass
]

{ #category : #'*rpackage-core' }
CompiledMethod >> isExtension [
	^ self origin package ~= self package
]

{ #category : #'*rpackage-core' }
CompiledMethod >> isExtensionInPackage: anRPackage [
	^ anRPackage includesExtensionSelector: self selector ofClass: self methodClass
]
