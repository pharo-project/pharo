"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestToolRegistry',
	#superclass : 'PackageManifest',
	#category : 'Tool-Registry-Manifest',
	#package : 'Tool-Registry',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestToolRegistry class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Graphics-Files' #'Kernel-CodeModel' #MenuRegistration)
]
