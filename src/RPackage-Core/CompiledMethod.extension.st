Extension { #name : #CompiledMethod }

{ #category : #'*RPackage-Core' }
CompiledMethod >> isDefinedInPackage: anRPackage [
	^ anRPackage includesDefinedSelector: self selector ofClass: self methodClass
]

{ #category : #'*RPackage-Core' }
CompiledMethod >> isExtensionInPackage: anRPackage [
	^ anRPackage includesExtensionSelector: self selector ofClass: self methodClass
]
