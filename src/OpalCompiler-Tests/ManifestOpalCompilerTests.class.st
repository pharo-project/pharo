Class {
	#name : #ManifestOpalCompilerTests,
	#superclass : #PackageManifest,
	#category : #'OpalCompiler-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestOpalCompilerTests >> ruleExtraBlockRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#OCContextTempMappingTest #testAccessingMultipleVariablesInVector #false)) #'2015-04-09T21:02:04.935487+02:00') #(#(#RGMethodDefinition #(#OCContextTempMappingTest #testAccessingTempsVectorInBlock #false)) #'2015-04-09T21:02:04.935425+02:00') #(#(#RGMethodDefinition #(#OCContextTempMappingTest #testAccessingVariablesInBlock #false)) #'2015-04-09T21:02:04.935457+02:00') )
]

{ #category : #'code-critics' }
ManifestOpalCompilerTests >> ruleGuardClauseRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#OCContextTempMappingTest #testAccessingVariablesInOptimizedBlock2 #false)) #'2015-04-09T21:02:04.919962+02:00') )
]

{ #category : #'code-critics' }
ManifestOpalCompilerTests >> ruleOnlyReadOrWrittenTemporaryRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#OCContextTempMappingTest #testAccessingVariablesInOptimizedBlock2 #false)) #'2015-04-09T21:02:04.928132+02:00') #(#(#RGMethodDefinition #(#OCContextTempMappingTest #testAccessingTempsVectorInBlock #false)) #'2015-04-09T21:02:04.927985+02:00') #(#(#RGMethodDefinition #(#OCContextTempMappingTest #testAccessingVariablesInOptimizedBlock #false)) #'2015-04-09T21:02:04.927828+02:00') #(#(#RGMethodDefinition #(#OCContextTempMappingTest #testAccessingVariablesInBlock #false)) #'2015-04-09T21:02:04.928107+02:00') )
]

{ #category : #'code-critics' }
ManifestOpalCompilerTests >> ruleTempsReadBeforeWrittenRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#OCContextTempMappingTest #testAccessingTempsVectorInBlock2 #false)) #'2015-04-09T21:02:04.945558+02:00') )
]
