"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestDebuggerModel',
	#superclass : 'PackageManifest',
	#category : 'Debugger-Model-Manifest',
	#package : 'Debugger-Model',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestDebuggerModel class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Object Events' #'Tool-Base' #'Morphic-Core' #DebugInfo #'OpalCompiler-Core')
]
