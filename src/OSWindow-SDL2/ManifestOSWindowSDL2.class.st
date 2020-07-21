"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestOSWindowSDL2,
	#superclass : #PackageManifest,
	#category : #'OSWindow-SDL2-Manifest'
}

{ #category : #'code-critics' }
ManifestOSWindowSDL2 class >> ruleUnaryAccessingMethodWithoutReturnRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#OSSDL2BackendWindow #hide #false)) #'2020-07-20T09:01:23.699228+02:00') )
]
