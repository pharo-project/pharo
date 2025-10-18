"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestSystemBenchmarkTests',
	#superclass : 'PackageManifest',
	#category : 'System-Benchmark-Tests-Manifest',
	#package : 'System-Benchmark-Tests',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemBenchmarkTests class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Time' #'Math-Operations-Extensions')
]
