"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestQualityAssistantRecording,
	#superclass : #PackageManifest,
	#category : 'QualityAssistantRecording-Manifest'
}

{ #category : #release }
ManifestQualityAssistantRecording class >> preUnload [

	QAEventCollector unload
]
