"
Please comment package here 
"
Class {
	#name : #ManifestGTEventRecorder,
	#superclass : #PackageManifest,
	#category : #'GT-EventRecorder-Manifest'
}

{ #category : #deprecation }
ManifestGTEventRecorder class >> isDeprecated [
	"This project will be removed of Pharo and should not be used anymore.
	
	If you wish to replace it, you can use: https://github.com/pharo-contributions/EventRecorder"

	^ true
]
