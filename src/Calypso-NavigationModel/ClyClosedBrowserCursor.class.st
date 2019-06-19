"
I represent closed query cursor.
I am used by data source to represent closed state.

My singleton instance is supposed to be used:

	ClyClosedBrowserCursor instance
"
Class {
	#name : #ClyClosedBrowserCursor,
	#superclass : #Object,
	#classInstVars : [
		'instance'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #accessing }
ClyClosedBrowserCursor class >> instance [
	^instance ifNil: [ instance := self new ]
]

{ #category : #controlling }
ClyClosedBrowserCursor >> close [
]

{ #category : #queries }
ClyClosedBrowserCursor >> findItemsWith: aCollection [ 
	^Array new: aCollection size withAll: nil
]

{ #category : #testing }
ClyClosedBrowserCursor >> hasMetaProperty: aPropertyClass [ 
	^false
]

{ #category : #accessing }
ClyClosedBrowserCursor >> itemCount [
	^0
]

{ #category : #controlling }
ClyClosedBrowserCursor >> updateItemCache [
]
