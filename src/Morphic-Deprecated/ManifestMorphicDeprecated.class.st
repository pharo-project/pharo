"
Contains deprecated Morphic code
"
Class {
	#name : 'ManifestMorphicDeprecated',
	#superclass : 'PackageManifest',
	#category : 'Morphic-Deprecated-Manifest',
	#package : 'Morphic-Deprecated',
	#tag : 'Manifest'
}

{ #category : 'deprecation' }
ManifestMorphicDeprecated class >> isDeprecated [

	^ true
]

{ #category : 'meta-data - dependency analyser' }
ManifestMorphicDeprecated class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Object Events')
]
