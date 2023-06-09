"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestAthensBalloon,
	#superclass : #PackageManifest,
	#category : #'Athens-Balloon-Manifest'
}

{ #category : #'code-critics' }
ManifestAthensBalloon class >> ruleUnaryAccessingMethodWithoutReturnRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#AthensBalloonImagePaint #repeat #false)) #'2021-06-21T23:01:31.97848+02:00') )
]

{ #category : #'code-critics' }
ManifestAthensBalloon class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#AthensBalloonPaint #asAthensPaintOn: #false)) #'2022-08-21T22:55:57.012722+02:00') #(#(#RGMethodDefinition #(#AthensBalloonPaint #asStrokePaintOn: #false)) #'2022-08-21T22:56:01.534141+02:00') )
]
