Extension { #name : 'MCScriptDefinition' }

{ #category : '*Deprecated12' }
MCScriptDefinition >> packageSet [
	self deprecated: 'I represent a unique package, thus I should not use a package set. Use #systemPackage instead.'.
	^ RPackageSet named: packageName
]
