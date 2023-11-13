"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestMonticelloModel',
	#superclass : 'PackageManifest',
	#category : 'Monticello-Model-Manifest',
	#package : 'Monticello-Model',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestMonticelloModel class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'OpalCompiler-Core' #'Kernel-Chronology-Extras' #'Collections-Abstract' #'Slot-Core' #'Collections-Streams' #'System-Support' #'Shift-ClassBuilder' #'FFI-Kernel' #'Shift-ClassInstaller' #'System-Changes')
]
