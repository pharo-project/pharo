Extension { #name : #CompiledMethod }

{ #category : #'*Nautilus' }
CompiledMethod >> dropIn: aRPackage forNautilusUI: aNautilusUI [
	aNautilusUI moveMethod: self inPackage: aRPackage.
	aNautilusUI updateCategoryAndMethodList
]
