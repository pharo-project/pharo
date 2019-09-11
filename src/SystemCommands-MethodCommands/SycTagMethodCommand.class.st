"
I am a command to classify given methods with given tag.

Internal Representation and Key Implementation Points.

    Instance Variables
	targetTag:		<Symbol>
"
Class {
	#name : #SycTagMethodCommand,
	#superclass : #SycMethodCommand,
	#instVars : [
		'targetTag'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #'instance creation' }
SycTagMethodCommand class >> for: methods tag: aString [

	^(self for: methods)
		targetTag: aString
]

{ #category : #execution }
SycTagMethodCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showMethodTag: targetTag
]

{ #category : #accessing }
SycTagMethodCommand >> defaultMenuIconName [
	^#glamorousBrowse

]

{ #category : #accessing }
SycTagMethodCommand >> defaultMenuItemName [
	^'Classify methods'
]

{ #category : #execution }
SycTagMethodCommand >> execute [
	
	methods do: [ :aMethod | 
		| oldTags |
		oldTags := aMethod tags.
		aMethod tagWith: targetTag asSymbol.
		oldTags do: [:old | aMethod untagFrom: old ]]
]

{ #category : #execution }
SycTagMethodCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	targetTag := aToolContext requestSingleMethodTag: 'New protocol name'
]

{ #category : #accessing }
SycTagMethodCommand >> targetTag [
	^ targetTag
]

{ #category : #accessing }
SycTagMethodCommand >> targetTag: anObject [
	targetTag := anObject
]
