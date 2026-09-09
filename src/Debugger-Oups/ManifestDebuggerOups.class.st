"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestDebuggerOups',
	#superclass : 'PackageManifest',
	#category : 'Debugger-Oups-Manifest',
	#package : 'Debugger-Oups',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestDebuggerOups class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#Traits #'Kernel-CodeModel' #'Transcript-NonInteractive' #'System-Support')
]
