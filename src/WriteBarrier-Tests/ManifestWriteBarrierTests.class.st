Class {
	#name : #ManifestWriteBarrierTests,
	#superclass : #PackageManifest,
	#category : #'WriteBarrier-Tests'
}

{ #category : #'code-critics' }
ManifestWriteBarrierTests class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGPackage #(#'WriteBarrier-Tests')) #'2018-01-07T18:19:17.97028+01:00') )
]
