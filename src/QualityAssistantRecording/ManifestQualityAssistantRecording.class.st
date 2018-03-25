Class {
	#name : #ManifestQualityAssistantRecording,
	#superclass : #PackageManifest,
	#category : #'QualityAssistantRecording-Manifest'
}

{ #category : #release }
ManifestQualityAssistantRecording class >> preUnload [

	QAEventCollector unload
]
