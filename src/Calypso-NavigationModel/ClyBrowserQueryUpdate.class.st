"
I represent information update from query result instance. I include updated metadata, total result size and item cache starting from particular position.

I was introduced to update cursor cache and metadata together by single message. It is important for remote scenario where query rsult is remote proxy and any interaction with it leads to network communication. I return all updated information in one request by single remote message.

Create my instances using following message:
	ClyBrowserQueryUpdate of: aBrowserQueryResult withItems: aBrowserQueryCache

Internal Representation and Key Implementation Points.

    Instance Variables
	itemCache:		<ClyBrowserQueryCache>
	metadata:		<ClyQueryResultMetadata>
	totalItemCount: <Integer>
"
Class {
	#name : #ClyBrowserQueryUpdate,
	#superclass : #Object,
	#instVars : [
		'metadata',
		'itemCache',
		'totalItemCount'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClyBrowserQueryUpdate class >> of: aBrowserQueryResult withItems: aBrowserQueryCache [
	^self new 
		metadata: aBrowserQueryResult metadata;
		itemCache: aBrowserQueryCache;
		totalItemCount: aBrowserQueryResult size
]

{ #category : #accessing }
ClyBrowserQueryUpdate >> itemCache [
	^ itemCache
]

{ #category : #accessing }
ClyBrowserQueryUpdate >> itemCache: anObject [
	itemCache := anObject
]

{ #category : #accessing }
ClyBrowserQueryUpdate >> metadata [
	^ metadata
]

{ #category : #accessing }
ClyBrowserQueryUpdate >> metadata: anObject [
	metadata := anObject
]

{ #category : #accessing }
ClyBrowserQueryUpdate >> totalItemCount [
	^ totalItemCount
]

{ #category : #accessing }
ClyBrowserQueryUpdate >> totalItemCount: anObject [
	totalItemCount := anObject
]
