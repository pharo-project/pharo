"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestRefactoringDataForTesting',
	#superclass : 'PackageManifest',
	#category : 'Refactoring-DataForTesting-Manifest',
	#package : 'Refactoring-DataForTesting',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestRefactoringDataForTesting class >> ruleClassNotReferencedRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#RBHierarchySubclassBForClassVariable)) #'2024-07-18T12:39:47.723158+02:00') #(#(#RGClassDefinition #(#MyClassNonEmptyLeafUnused)) #'2024-07-18T12:40:19.70241+02:00') #(#(#RGClassDefinition #(#RBDummyCompositeLintRuleTest)) #'2024-07-18T12:40:19.702201+02:00') #(#(#RGClassDefinition #(#RBClassUsingSharedPoolForTestData)) #'2024-07-18T12:40:19.701335+02:00') #(#(#RGClassDefinition #(#RBBasicDummyLintRuleTest)) #'2024-07-18T12:40:19.702472+02:00') #(#(#RGClassDefinition #(#RBTransformationRuleTestData2)) #'2024-07-18T12:40:19.701755+02:00') )
]

{ #category : 'code-critics' }
ManifestRefactoringDataForTesting class >> ruleEquivalentSuperclassMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#MySubAccessingSuperclassState #accessingSharedVariable #false)) #'2024-07-18T12:38:46.533561+02:00') #(#(#RGMethodDefinition #(#RBTransformationDummyRuleTest1 #openEditor #false)) #'2024-07-18T12:38:46.533586+02:00') #(#(#RGMethodDefinition #(#MySubAccessingSuperclassState #accessing #false)) #'2024-07-18T12:38:46.533614+02:00') #(#(#RGMethodDefinition #(#RBSharedPoolForTestData1 #msg4 #false)) #'2024-07-18T12:38:46.532822+02:00') #(#(#RGMethodDefinition #(#RBEquivalentMethodSubclassHolder #yourself #false)) #'2024-07-18T12:38:46.533594+02:00') #(#(#RGMethodDefinition #(#RBSubclassOfClassToRename #equallyDefinedInSubclass #false)) #'2024-07-18T12:38:46.533603+02:00') #(#(#RGMethodDefinition #(#RBSharedPoolForTestData2 #msg4 #false)) #'2024-07-18T12:38:46.533433+02:00') )
]

{ #category : 'code-critics' }
ManifestRefactoringDataForTesting class >> ruleExcessiveArgumentsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#RBBasicLintRuleTestData #method:with:lots:of:arguments: #false)) #'2024-07-18T12:40:39.143779+02:00') #(#(#RGMethodDefinition #(#RBBasicDummyLintRuleTest #method:with:lots:of:arguments: #false)) #'2024-07-18T12:40:39.143696+02:00') )
]

{ #category : 'code-critics' }
ManifestRefactoringDataForTesting class >> ruleExcessiveMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#RBClassDataForRefactoringTest)) #'2024-07-18T12:41:02.656436+02:00') #(#(#RGMetaclassDefinition #(#'RBBasicDummyLintRuleTest class' #RBBasicDummyLintRuleTest)) #'2024-07-18T12:41:02.656083+02:00') )
]

{ #category : 'code-critics' }
ManifestRefactoringDataForTesting class >> ruleNoUnusedInstanceVariableRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ReClassForGeneratingEqualAndHash)) #'2024-07-05T12:05:01.988597+02:00') #(#(#RGClassDefinition #(#ReClassForGeneratingEqualAndHashExistingImplementors)) #'2024-07-05T12:07:30.662537+02:00') )
]

{ #category : 'code-critics' }
ManifestRefactoringDataForTesting class >> ruleReClassVariableNeitherReadNorWrittenRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#RBHierarchySubclassAForClassVariable)) #'2024-07-18T12:50:23.92547+02:00') #(#(#RGClassDefinition #(#RBHierarchySubclassBForClassVariable)) #'2024-07-18T12:50:42.673173+02:00') #(#(#RGMetaclassDefinition #(#'RBHierarchySubclassBForClassVariable class' #RBHierarchySubclassBForClassVariable)) #'2024-07-18T14:51:42.347867+02:00') #(#(#RGMetaclassDefinition #(#'RBHierarchySubclassAForClassVariable class' #RBHierarchySubclassAForClassVariable)) #'2024-07-18T14:51:42.347564+02:00') )
]

{ #category : 'code-critics' }
ManifestRefactoringDataForTesting class >> ruleReIvarNeitherReadNorWrittenRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ReClassForGeneratingPrintOn)) #'2024-06-20T19:43:13.245998+02:00') #(#(#RGClassDefinition #(#ReClassForGeneratingEqualAndHashExistingImplementors)) #'2024-06-20T19:43:13.245838+02:00') #(#(#RGClassDefinition #(#ReClassForGeneratingEqualAndHash)) #'2024-06-20T19:43:13.245672+02:00') )
]

{ #category : 'code-critics' }
ManifestRefactoringDataForTesting class >> ruleRefersToClassRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'RBBasicLintRuleTestData class' #modifiesCollection #true)) #'2024-07-18T12:42:35.357229+02:00') #(#(#RGMethodDefinition #(#RBClassDataForRefactoringTest #refersToClass #false)) #'2024-07-18T12:42:35.357361+02:00') )
]

{ #category : 'code-critics' }
ManifestRefactoringDataForTesting class >> ruleSendsDifferentSuperRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#RBEquivalentMethodSubclassHolder #methodToExtractFromWithSuperAndSimilarMethodInSuper #false)) #'2024-07-18T12:41:57.585437+02:00') #(#(#RGMethodDefinition #(#RBEquivalentMethodRootHolder #methodWithSuper #false)) #'2024-07-18T12:41:57.585423+02:00') #(#(#RGMethodDefinition #(#RBEquivalentMethodSubclassHolder #methodWithSuperSend #false)) #'2024-07-18T12:41:57.585283+02:00') #(#(#RGMethodDefinition #(#RBEquivalentMethodSubclassHolder #methodToExtractFrom #false)) #'2024-07-18T12:41:57.585158+02:00') #(#(#RGMethodDefinition #(#RBClassDataForRefactoringTest #sendsDifferentSuper #false)) #'2024-07-18T12:41:57.585447+02:00') #(#(#RGMethodDefinition #(#RBSubclassOfClassToRename #justSuperSendInSubclassBroken #false)) #'2024-07-18T12:41:57.585456+02:00') )
]
