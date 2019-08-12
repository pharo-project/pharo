"
I provide tagged method groups. 
I collect all tags from class scope and create ClyTaggedMethodGroup instances for each of them.

I skip any tag which represent class extension, according to star convention('*PackageName'). Class extensions are handled by different provider: ClyExtendedMethodGroupProvider. It creates only general ""extensions"" group which can be expanded by subgroups for each package which extends given class. Subgroups are represented by explicit ClyExternalPackageMethodGroup. No star-convension is needed here
"
Class {
	#name : #ClyTaggedMethodGroupProvider,
	#superclass : #ClyMethodGroupProvider,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'building groups' }
ClyTaggedMethodGroupProvider >> buildGroupsFrom: aClassScope [
	| groups |
	groups := IdentityDictionary new.
	
	aClassScope classesDo: [ :eachClass |
		eachClass tagsForAllMethods do: [ :eachTag | 
			groups at: eachTag ifAbsentPut: [ 
				ClyTaggedMethodGroup withMethodsFrom: aClassScope taggedBy: eachTag ] ] ].

	^ groups values
]

{ #category : #'system changes' }
ClyTaggedMethodGroupProvider >> providesGroupsAffectedBy: aSystemAnnouncement inScope: aClassScope [

	aClassScope classesDo: [ :each | 
		(aSystemAnnouncement affectsMethodTagIn: each) ifTrue: [ ^true ]
	].

	^false
]
