"
I install the deprecated transformation meta-links within the AST.
"
Class {
	#name : #ClassDeprecationMetaLinkInstaller,
	#superclass : #RBProgramNodeVisitor,
	#category : #ClassDeprecation
}

{ #category : #visiting }
ClassDeprecationMetaLinkInstaller >> visitGlobalNode: aNode [
	super visitGlobalNode: aNode.
	aNode isDeprecatedTransformable
		ifTrue: [ aNode installDeprecationTransformMetaLink ]
]
