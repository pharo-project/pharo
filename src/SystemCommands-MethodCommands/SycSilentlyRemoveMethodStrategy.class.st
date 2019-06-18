"
I am simple perform method remove operation without any extra logic
"
Class {
	#name : #SycSilentlyRemoveMethodStrategy,
	#superclass : #SycRemoveMethodStrategy,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #removing }
SycSilentlyRemoveMethodStrategy >> collectMethodTagsFrom: methods [
	"It will return map class->tags where tags are releated to given methods"
	| result tags |
	result := IdentityDictionary new.
	methods do: [ :each | 
		tags := result at: each origin ifAbsentPut: [IdentitySet new].
		tags addAll: each tags].
	^result
]

{ #category : #removing }
SycSilentlyRemoveMethodStrategy >> removeMethods: methods [
	
	| methodTags |
	methodTags := self collectMethodTagsFrom: methods. "we will remove empty tags of removed methods".
	
	methods do: [ :each | each removeFromSystem ].
	
	methodTags keysAndValuesDo: [:class :tags | tags do: [ :eachTag | 
		(class methodsTaggedWith: eachTag) 
			ifEmpty: [class removeMethodTag: eachTag] ]]
]

{ #category : #removing }
SycSilentlyRemoveMethodStrategy >> userRequestString [
	^'Remove it'
]
