"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestShiftClassBuilderTests',
	#superclass : 'PackageManifest',
	#category : 'Shift-ClassBuilder-Tests-Manifest',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestShiftClassBuilderTests class >> ruleClassNotReferencedRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ShCBSubClassWithInstanceVariable)) #'2025-06-04T15:25:18.880697+02:00') #(#(#RGClassDefinition #(#ShCBAClasWithAnSlot)) #'2025-06-04T15:26:02.447018+02:00') #(#(#RGClassDefinition #(#ShCBAClasWithAnotherSlot)) #'2025-06-05T10:19:19.373201+02:00') #(#(#RGClassDefinition #(#ShCBSubSubClassWithInstanceVariable)) #'2025-06-05T10:19:30.306238+02:00') )
]

{ #category : 'code-critics' }
ManifestShiftClassBuilderTests class >> ruleNoUnusedInstanceVariableRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ShCBSubClassWithInstanceVariable)) #'2025-06-04T15:25:21.900882+02:00') #(#(#RGClassDefinition #(#ShCBAClasWithAnSlot)) #'2025-06-04T15:25:57.776847+02:00') #(#(#RGClassDefinition #(#ShCBSubSubClassWithInstanceVariable)) #'2025-06-05T10:19:35.137015+02:00') #(#(#RGClassDefinition #(#ShCBAClasWithAnotherSlot)) #'2025-06-05T10:19:41.196766+02:00') )
]

{ #category : 'code-critics' }
ManifestShiftClassBuilderTests class >> ruleReIvarNeitherReadNorWrittenRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ShCBSubClassWithInstanceVariable)) #'2025-06-04T15:25:13.660941+02:00') #(#(#RGClassDefinition #(#ShCBAClasWithAnSlot)) #'2025-06-04T15:26:00.30664+02:00') #(#(#RGClassDefinition #(#ShCBAClasWithAnotherSlot)) #'2025-06-05T10:19:22.809563+02:00') #(#(#RGClassDefinition #(#ShCBSubSubClassWithInstanceVariable)) #'2025-06-05T10:19:32.863354+02:00') )
]
