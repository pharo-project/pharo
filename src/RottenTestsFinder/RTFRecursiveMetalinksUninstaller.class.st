"
I recursively uninstall metalinks in all helpers methods directly reachable from the node where the visit is started.

Also set the Metalink stored in the RTFSelfCallNode I visit to nil after uninstallation.
"
Class {
	#name : 'RTFRecursiveMetalinksUninstaller',
	#superclass : 'RTFSelfCallVisitor',
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'private' }
RTFRecursiveMetalinksUninstaller >> uninstallMetaLinkOf: aRTFSelfCallNode [
	aRTFSelfCallNode metaLink uninstall.
	aRTFSelfCallNode metaLink: nil
]

{ #category : 'visiting' }
RTFRecursiveMetalinksUninstaller >> visitRTFSelfCallNode: aRTFSelfCallNode [
	self uninstallMetaLinkOf: aRTFSelfCallNode.
	aRTFSelfCallNode subCalls do: [ :selfCallSubNode |
		selfCallSubNode acceptVisitor: self ]
]

{ #category : 'visiting' }
RTFRecursiveMetalinksUninstaller >> visitRTFSelfCallRootNode: aRTFSelfCallRootNode [
	aRTFSelfCallRootNode subCalls do: [ :subSelfCall |
		subSelfCall acceptVisitor: self ]
]
