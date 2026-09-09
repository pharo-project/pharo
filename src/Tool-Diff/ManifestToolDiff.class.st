"
Package to support displaying changes in the form of diffs for tools
"
Class {
	#name : 'ManifestToolDiff',
	#superclass : 'PackageManifest',
	#category : 'Tool-Diff-Manifest',
	#package : 'Tool-Diff',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestToolDiff class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Object Events' #'System-Localization')
]
