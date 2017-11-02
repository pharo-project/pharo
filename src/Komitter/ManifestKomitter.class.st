Class {
	#name : #ManifestKomitter,
	#superclass : #PackageManifest,
	#category : 'Komitter'
}

{ #category : #code-critics }
ManifestKomitter >> ruleMissingYourselfRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#KomitSliceUI #initializeWidgets #false)) #'2015-05-07T14:07:56.036149-04:00') )
]
