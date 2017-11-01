Extension { #name : #TheManifestBuilder }

{ #category : #'*Tool-DependencyAnalyser-UI' }
TheManifestBuilder >> addManuallyResolvedDependency: aPackageName [
	| packageNames |	
	
	packageNames := self manifest manuallyResolvedDependencies asSet.
	packageNames add: aPackageName asSymbol.
	packageNames := packageNames asArray.
	self 
		compileSelector: #manuallyResolvedDependencies
		returnValue: packageNames asString
		classified: 'meta-data - dependency analyser'
]
