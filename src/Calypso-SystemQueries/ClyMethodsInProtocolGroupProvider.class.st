"
I provide methods in protocol groups. 
I collect all protocols from class scope and create ClyMethodsInProtocolGroue instances for each of them.

I skip any protocol which represent class extension, according to star convention('*PackageName'). Class extensions are handled by different provider: ClyExtendedMethodGroupProvider. It creates only general ""extensions"" group which can be expanded by subgroups for each package which extends given class. Subgroups are represented by explicit ClyExternalPackageMethodGroup. No star-convension is needed here
"
Class {
	#name : 'ClyMethodsInProtocolGroupProvider',
	#superclass : 'ClyMethodGroupProvider',
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'building groups' }
ClyMethodsInProtocolGroupProvider >> buildGroupsFrom: aClassScope [
	| groups |
	groups := IdentityDictionary new.

	aClassScope classesDo: [ :eachClass |
		eachClass protocolsForAllMethods do: [ :protocol |
			groups at: protocol ifAbsentPut: [
				ClyMethodsInProtocolGroup withMethodsFrom: aClassScope inProtocol: protocol ] ] ].

	^ groups values
]

{ #category : 'system changes' }
ClyMethodsInProtocolGroupProvider >> providesGroupsAffectedBy: aSystemAnnouncement inScope: aClassScope [

	aClassScope classesDo: [ :each | (aSystemAnnouncement affectsProtocolsIn: each) ifTrue: [ ^ true ] ].

	^ false
]
