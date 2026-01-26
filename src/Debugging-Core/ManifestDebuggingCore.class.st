"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestDebuggingCore',
	#superclass : 'PackageManifest',
	#category : 'Debugging-Core-Manifest',
	#package : 'Debugging-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestDebuggingCore class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Collections-Streams' #'Collections-Abstract' #'OpalCompiler-Core' #'System-NumberPrinting' #'AST-Core' #DebugInfo #'Kernel-BytecodeEncoders')
]
