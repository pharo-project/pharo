"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestLibGitCore,
	#superclass : #PackageManifest,
	#category : #'LibGit-Core-Manifest'
}

{ #category : #'meta data' }
ManifestLibGitCore class >> rejectClasses [
^ #()
]

{ #category : #'meta data' }
ManifestLibGitCore class >> rejectRules [
^ #()
]

{ #category : #'meta data' }
ManifestLibGitCore class >> ruleIfTrueBlocksRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#LGitExternalObject #ifNotReady: #false)) #'2014-11-14T16:52:32.831129+01:00') )
]

{ #category : #'meta data' }
ManifestLibGitCore class >> ruleIfTrueReturnsRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#LGitReference #= #false)) #'2014-11-14T16:52:32.91457+01:00') #(#(#RGMethodDefinition #(#LGitReference #= #false)) #'2014-11-14T16:52:32.915066+01:00') )
]

{ #category : #'code-critics' }
ManifestLibGitCore class >> ruleRBAbstractClassRuleV1FalsePositive [
^ #(#(#(#RGMetaclassDefinition #(#'LGitExternalEnumerationUInt32 class' #LGitExternalEnumerationUInt32)) #'2015-07-18T14:21:49.402159+02:00') )
]

{ #category : #'code-critics' }
ManifestLibGitCore class >> ruleRBNoClassCommentRuleV1FalsePositive [
^ #(#(#(#RGClassDefinition #(#LGitPushOptions)) #'2015-07-17T12:59:06.388548+02:00') )
]
