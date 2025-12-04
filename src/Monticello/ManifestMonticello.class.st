"
Includes the Monticello source code control system.
"
Class {
	#name : 'ManifestMonticello',
	#superclass : 'PackageManifest',
	#category : 'Monticello-Manifest',
	#package : 'Monticello',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestMonticello class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#Jobs #'Random-Core' #'System-Support')
]
