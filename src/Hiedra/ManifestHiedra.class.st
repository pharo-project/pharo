"
Small Pharo project to visually connect items in a history-like graph. It uses Athens, and provides support for Morphic and Spec.
"
Class {
	#name : #ManifestHiedra,
	#superclass : #PackageManifest,
	#category : #'Hiedra-Manifest'
}

{ #category : #'code-critics' }
ManifestHiedra class >> ruleBadMessageRule2V1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'HiSpecExample class' #openExample: #true)) #'2019-03-15T17:53:30.590665-03:00') #(#(#RGMethodDefinition #(#'HiFastTableExample class' #openExample: #true)) #'2019-03-15T17:53:30.590768-03:00') )
]

{ #category : #'code-critics' }
ManifestHiedra class >> ruleModifiesCollectionRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'HiExampleCommit class' #randomlyGenerated #true)) #'2019-03-15T17:53:30.606135-03:00') )
]

{ #category : #'code-critics' }
ManifestHiedra class >> ruleRBToDoCollectRuleV1FalsePositive [
	^ #(#(#(#RGPackage #(#Hiedra)) #'2019-03-15T18:30:14.575162-03:00') )
]
