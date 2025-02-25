"
I recursively install metalinks in all helpers methods directly reachable from the node where the visit is started.

Stores the Metalink in RTFSelfCallNode I visit.
"
Class {
	#name : 'RTFRecursiveMetalinksInstaller',
	#superclass : 'RTFSelfCallVisitor',
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'private' }
RTFRecursiveMetalinksInstaller >> installMetaLinkOn: aRTFSelfCallNode [
	|metalink|
	metalink := MetaLink new
					metaObject: aRTFSelfCallNode;
					selector: #markAsExecuted;
					yourself.
	aRTFSelfCallNode astNode link: metalink.
	aRTFSelfCallNode metaLink: metalink
]

{ #category : 'visiting' }
RTFRecursiveMetalinksInstaller >> visitRTFSelfCallNode: aRTFSelfCallNode [
	self installMetaLinkOn: aRTFSelfCallNode.
	aRTFSelfCallNode subCalls do: [ :selfCallSubNode |
		selfCallSubNode acceptVisitor: self. ]
]

{ #category : 'visiting' }
RTFRecursiveMetalinksInstaller >> visitRTFSelfCallRootNode: aRTFSelfCallRootNode [
	aRTFSelfCallRootNode subCalls do: [ :subSelfCall |
		subSelfCall acceptVisitor: self ]
]
