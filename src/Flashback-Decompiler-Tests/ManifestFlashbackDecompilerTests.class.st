"
Includes unit tests for Flashback - a Decomplier for Pharo code
"
Class {
	#name : #ManifestFlashbackDecompilerTests,
	#superclass : #PackageManifest,
	#category : #'Flashback-Decompiler-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleAssignmentInIfTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FBDExamples #exampleIfFalseIfTrue2 #false)) #'2020-10-23T22:09:15.457352+02:00') )
]

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleBadMessageRule2V1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FBDExamples #exampleCaseOf #false)) #'2020-10-23T22:05:42.169628+02:00') #(#(#RGMethodDefinition #(#FBDExamples #exampleCaseOf2 #false)) #'2020-10-23T22:05:56.503687+02:00') #(#(#RGMethodDefinition #(#FBDExamples #exampleCaseOf3 #false)) #'2020-10-23T22:06:12.663971+02:00') #(#(#RGMethodDefinition #(#FBDExamples #exampleCaseOf4 #false)) #'2020-10-23T22:06:37.803427+02:00') #(#(#RGMethodDefinition #(#FBDExamples #exampleCaseOf5 #false)) #'2020-10-23T22:06:52.696106+02:00') #(#(#RGMethodDefinition #(#FBDExamples #exampleCaseOf7 #false)) #'2020-10-23T22:07:20.994769+02:00') #(#(#RGMethodDefinition #(#FBDExamples #exampleCaseOf8 #false)) #'2020-10-23T22:07:27.684839+02:00') #(#(#RGMethodDefinition #(#FBDExamples #exampleCaseOfOtherwise #false)) #'2020-10-23T22:07:55.351219+02:00') )
]

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleDeadBlockRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FBDExamples #noRemoteReadInBlock #false)) #'2020-10-23T22:26:51.217826+02:00') #(#(#RGMethodDefinition #(#FBDExamples #noRemoteReadNestedBlocks #false)) #'2020-10-23T22:27:11.025787+02:00') #(#(#RGMethodDefinition #(#FBDExamples #singleRemoteReadNestedBlocks #false)) #'2020-10-23T22:28:30.113023+02:00') )
]

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleExtraBlockRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#FBDExamples)) #'2020-10-23T22:01:24.485282+02:00') )
]

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleGuardingClauseRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FBDExamples #exampleIfTrueForEffectNested #false)) #'2020-10-23T22:11:42.525199+02:00') )
]

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleMinMaxRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FBDExamples #exampleIfTrueAssign #false)) #'2020-10-23T22:11:22.078885+02:00') )
]

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleReGuardClauseRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FBDExamples #exampleIfTrueForEffectNested #false)) #'2020-10-23T22:11:39.607515+02:00') )
]

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleReturnsIfTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FBDExamples #exampleIfFalse2 #false)) #'2020-10-23T22:08:49.69856+02:00') #(#(#RGMethodDefinition #(#FBDExamples #exampleIfTrue2 #false)) #'2020-10-23T22:11:05.040374+02:00') #(#(#RGMethodDefinition #(#FBDExamples #exampleSimpleBlockLocalIfNested #false)) #'2020-10-23T22:19:41.855023+02:00') )
]

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleToDoCollectRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FBDExamples #exampleToDo3 #false)) #'2020-10-23T22:21:30.445095+02:00') )
]

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleUsesAddRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FBDExamples #exampleCascadeIfTrueIfFalse2 #false)) #'2020-10-23T22:04:02.8472+02:00') )
]

{ #category : #'code-critics' }
ManifestFlashbackDecompilerTests class >> ruleWhileTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FBDExamples #exampleToDo #false)) #'2020-10-23T22:21:15.909488+02:00') #(#(#RGMethodDefinition #(#FBDExamples #exampleWhileTrue #false)) #'2020-10-23T22:23:42.18587+02:00') )
]
