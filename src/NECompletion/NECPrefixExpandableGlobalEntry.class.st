Class {
	#name : 'NECPrefixExpandableGlobalEntry',
	#superclass : 'NECGlobalEntry',
	#instVars : [
		'implicitPrefix'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'as yet unclassified' }
NECPrefixExpandableGlobalEntry class >> contents: aString node: aNode implicitPrefix: aPrefix [

	^ self new
		contents: aString node: aNode;
		implicitPrefix: aPrefix;
		yourself
]

{ #category : 'matching' }
NECPrefixExpandableGlobalEntry >> completionMatchText [

	(NECPreferences expandPrefixes
		and: [ implicitPrefix isNotNil
		and: [ implicitPrefix notEmpty
		and: [ self contents size > implicitPrefix size
		and: [ self contents beginsWith: implicitPrefix ] ] ] ])
		ifTrue: [ ^ self contents allButFirst: implicitPrefix size ].

	^ super completionMatchText
]

{ #category : 'matching' }
NECPrefixExpandableGlobalEntry >> implicitPrefix [

	^ implicitPrefix
]

{ #category : 'matching' }
NECPrefixExpandableGlobalEntry >> implicitPrefix: aString [

	implicitPrefix := aString
]
