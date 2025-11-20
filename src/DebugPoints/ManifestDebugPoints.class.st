"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestDebugPoints',
	#superclass : 'PackageManifest',
	#category : 'DebugPoints-Manifest',
	#package : 'DebugPoints',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestDebugPoints class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'OpalCompiler-Core' #'Transcript-NonInteractive' #'Announcements-Core' #'Collections-Abstract' #MenuRegistration #'Debugging-Core')
]
