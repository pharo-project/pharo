"
I represent the declaration of a class organization in a file.  My contents is the class organization string to import, and the class organizer is the one of the changed class.

Sending me the message #import makes me install the class organization I carry into the class.
"
Class {
	#name : #ClassOrganizationChunk,
	#superclass : #BehaviorOwnedChunk,
	#category : #'CodeImport-Chunks'
}

{ #category : #'instance creation' }
ClassOrganizationChunk class >> contents: someContents behaviorName: behaviorName isMeta: aBoolean [
	^self new
		contents: someContents;
		behaviorName: behaviorName;
		isMeta: aBoolean;
		yourself
]

{ #category : #'instance creation' }
ClassOrganizationChunk class >> contents: someContents organizer: aClassOrganizer [
	^self new
			contents: someContents;
			organizer: aClassOrganizer;
			yourself
]

{ #category : #visiting }
ClassOrganizationChunk >> accept: aVisitor [

	^ aVisitor visitClassOrganizationChunk: self
]

{ #category : #accessing }
ClassOrganizationChunk >> description [

	^ 'Organization (', behaviorName, ')'
]

{ #category : #importing }
ClassOrganizationChunk >> importFor: aRequestor logSource: logSource [

	| organization protocolSpecs |
	self existsBehavior ifFalse: [ self error: ('Cannot change organization of unexistent behavior {1}' format: { behaviorName asString }) ].

	organization := self targetClass organization.
	protocolSpecs := contents parseLiterals.

	organization reset.

	"If nothing was scanned and I had no elements before, then default me"
	(protocolSpecs isEmpty and: [ organization protocols isEmpty ]) ifTrue: [ ^ self ].

	protocolSpecs do: [ :spec |
		| protocolName methods |
		protocolName := spec first asSymbol.
		methods := spec allButFirst asSet.
		organization addProtocol: (Protocol name: protocolName methodSelectors: methods) ]
]

{ #category : #testing }
ClassOrganizationChunk >> isClassOrganizationDeclaration [

	^ true
]
