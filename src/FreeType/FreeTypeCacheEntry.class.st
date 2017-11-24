"
An entry for the FreeTypeCache
"
Class {
	#name : #FreeTypeCacheEntry,
	#superclass : #Link,
	#instVars : [
		'font',
		'charCode',
		'type',
		'object',
		'previousLink'
	],
	#category : #'FreeType-Cache'
}

{ #category : #comparing }
FreeTypeCacheEntry >> = aFreeTypeCacheEntry [
	"equailty based on font,charcode, type, object, but not nextLink"
	
	(aFreeTypeCacheEntry isKindOf: FreeTypeCacheEntry) 
		ifFalse:[^false].
	^font = aFreeTypeCacheEntry font and: [
		charCode = aFreeTypeCacheEntry charCode
			and: [type = aFreeTypeCacheEntry type
				and:[object = aFreeTypeCacheEntry object]]]
]

{ #category : #accessing }
FreeTypeCacheEntry >> charCode [
	"Answer the value of charCode"

	^ charCode
]

{ #category : #accessing }
FreeTypeCacheEntry >> charCode: anObject [
	"Set the value of charCode"

	charCode := anObject
]

{ #category : #accessing }
FreeTypeCacheEntry >> font [
	"Answer the value of font"

	^ font
]

{ #category : #accessing }
FreeTypeCacheEntry >> font: anObject [
	"Set the value of font"

	font := anObject
]

{ #category : #comparing }
FreeTypeCacheEntry >> hash [
	^charCode hash
]

{ #category : #accessing }
FreeTypeCacheEntry >> object [
	"Answer the value of object"

	^ object
]

{ #category : #accessing }
FreeTypeCacheEntry >> object: anObject [
	"Set the value of object"

	object := anObject
]

{ #category : #accessing }
FreeTypeCacheEntry >> previousLink [
	"Answer the value of previousLink"

	^ previousLink
]

{ #category : #accessing }
FreeTypeCacheEntry >> previousLink: anObject [
	"Set the value of previousLink"

	previousLink := anObject
]

{ #category : #accessing }
FreeTypeCacheEntry >> type [
	"Answer the value of type"

	^ type
]

{ #category : #accessing }
FreeTypeCacheEntry >> type: anObject [
	"Set the value of type"

	type := anObject
]
