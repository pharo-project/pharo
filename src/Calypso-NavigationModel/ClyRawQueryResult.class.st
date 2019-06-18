"
I represent items as is. I do not apply any formatting or transformation.

I am default #requiredResult of any query.
"
Class {
	#name : #ClyRawQueryResult,
	#superclass : #ClyQueryResult,
	#category : #'Calypso-NavigationModel-Result'
}

{ #category : #building }
ClyRawQueryResult >> fillWith: queriedObjects [

	items addAll: queriedObjects
]
