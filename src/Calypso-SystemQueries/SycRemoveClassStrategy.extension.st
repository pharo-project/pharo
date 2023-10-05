Extension { #name : 'SycRemoveClassStrategy' }

{ #category : '*Calypso-SystemQueries' }
SycRemoveClassStrategy class >> createForBrowser: aBrowser classes: aCollection [

	| hasReferences hasSubclasses hasUsers |
	hasReferences := (ClyClassExternalReferencesQuery
		                  toAny: aCollection
		                  from: aBrowser systemScope) execute notEmpty.
	hasSubclasses := ((aCollection flatCollect: [ :each |
		                   each subclasses ]) copyWithoutAll: aCollection)
		                 notEmpty.
	hasUsers := ((aCollection flatCollect: [ :each | each users ])
		             copyWithoutAll: aCollection) notEmpty.

	hasReferences | hasSubclasses | hasUsers ifFalse: [
		^ { SycSilentlyRemoveClassStrategy new } ].

	^ self
		  createForBrowser: aBrowser
		  references: hasReferences
		  subclasses: hasSubclasses
		  users: hasUsers
]
