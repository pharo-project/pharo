Class {
	#name : 'ManifestOpalCompilerCore',
	#superclass : 'PackageManifest',
	#category : 'OpalCompiler-Core-Manifest',
	#package : 'OpalCompiler-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestOpalCompilerCore class >> ignoredDependencies [
	^ #(#'Morphic-Base' #'System-Settings-Core')
]

{ #category : 'meta-data - dependency analyser' }
ManifestOpalCompilerCore class >> manuallyResolvedDependencies [
	^ #(#'Morphic-Base' #'System-Settings-Core' #'System-Announcements' #'System-Sources')
]

{ #category : 'code-critics' }
ManifestOpalCompilerCore class >> rejectRules [
^ #('SearchingLiteralRule' 'ExcessiveArgumentsRule' 'LongMethodsRule' 'ClassInstVarNotInitializedRule' 'UsesAddRule' 'ExcessiveMethodsRule' 'MissingYourselfRule' 'ExcessiveVariablesRule' 'TempsReadBeforeWrittenRule' 'AbstractClassRule' 'IfTrueReturnsRule')
]

{ #category : 'code-critics' }
ManifestOpalCompilerCore class >> ruleAssignmentInIfTrueRuleV1TruePositive [
^ #(#(#(#RGMethodDefinition #(#OpalCompiler #from:class:context:notifying: #false)) #'2012-12-18T17:43:24.703000001+01:00') )
]

{ #category : 'code-critics' }
ManifestOpalCompilerCore class >> ruleBadMessageRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#OCASTTranslator #initialize #false)) #'2013-05-23T08:43:44.793471+02:00') #(#(#RGMethodDefinition #(#OCASTTranslator #emitCaseOf: #false)) #'2013-05-23T08:43:44.793471+02:00') #(#(#RGMethodDefinition #(#OpalCompiler #evaluate #false)) #'2013-05-23T08:43:44.793471+02:00') )
]

{ #category : 'code-critics' }
ManifestOpalCompilerCore class >> ruleClassNotReferencedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#OpalEncoderForLongFormV3PlusClosures)) #'2015-08-17T13:55:18.528985+02:00') )
]

{ #category : 'code-critics' }
ManifestOpalCompilerCore class >> ruleImplementedNotSentRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#OCASTTranslatorForEffect #emitIfFalse: #false)) #'2012-12-18T17:41:36.969+01:00') #(#(#RGMethodDefinition #(#OCASTTranslatorForEffect #emitIfTrue: #false)) #'2012-12-18T17:41:39.446+01:00') #(#(#RGMethodDefinition #(#OCASTTranslator #emitCaseOf: #false)) #'2012-12-18T17:42:01.724+01:00') #(#(#RGMethodDefinition #(#OCASTTranslator #emitToByDo: #false)) #'2012-12-18T17:42:03.557+01:00') #(#(#RGMethodDefinition #(#OCASTTranslator #emitToDo: #false)) #'2012-12-18T17:42:05.34+01:00') #(#(#RGMethodDefinition #(#OCASTTranslator #emitIfNotNil: #false)) #'2012-12-18T17:42:07.349+01:00') #(#(#RGMethodDefinition #(#OCASTTranslator #emitIfNil: #false)) #'2012-12-18T17:42:09.472+01:00') #(#(#RGMethodDefinition #(#OCASTTranslator #emitCaseOfOtherwise: #false)) #'2012-12-18T17:42:11.795+01:00') #(#(#RGMethodDefinition #(#BytecodeEncoder #sizeCallPrimitive: #false)) #'2015-08-17T13:56:21.549128+02:00') )
]

{ #category : 'code-critics' }
ManifestOpalCompilerCore class >> ruleTempsReadBeforeWrittenRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#OCIRSequence #absorbJumpToSingleInstr: #false)) #'2012-12-19T08:10:39.177+01:00') )
]

{ #category : 'code-critics' }
ManifestOpalCompilerCore class >> ruleToDoCollectRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#OCIRBytecodeScope #args #false)) #'2012-12-19T08:16:58.474000001+01:00') #(#(#RGMethodDefinition #(#OCIRBytecodeScope #temps #false)) #'2012-12-19T08:17:19.677+01:00') )
]

{ #category : 'code-critics' }
ManifestOpalCompilerCore class >> ruleVariableAssignedLiteralRuleV1FalsePositive [
^ #(#(#(#RGClassDefinition #(#OCIRMethod)) #'2012-12-19T08:13:54.508+01:00') )
]
