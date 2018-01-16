Extension { #name : #RPackage }

{ #category : #'*Ring2-Compatibility' }
RPackage >> asRing2Definition [
	^RG2PackageDefinition named: self name asSymbol
]

{ #category : #'*Ring2-Compatibility' }
RPackage >> asRing2DefinitionIn: aRG2Environment [

	| pkg parent|
	
	pkg := self asRing2MinimalDefinitionIn: aRG2Environment.
	
	parent := pkg findSuitableParentIn: aRG2Environment.
	pkg parent: parent.
	
	^ pkg
]
