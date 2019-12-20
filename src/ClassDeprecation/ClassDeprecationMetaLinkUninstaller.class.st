"
I uninstall the deprecated transformation meta-links within the AST.
"
Class {
	#name : #ClassDeprecationMetaLinkUninstaller,
	#superclass : #RBProgramNodeVisitor,
	#category : #ClassDeprecation
}

{ #category : #visiting }
ClassDeprecationMetaLinkUninstaller >> visitGlobalNode: aNode [
	super visitGlobalNode: aNode.
	aNode hasDeprecationTransformMetaLink
		ifTrue: [ aNode uninstallDeprecationTransformMetaLink ]
]
