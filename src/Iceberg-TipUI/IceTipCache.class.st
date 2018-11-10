"
I'm a cache that can be reseted when some announcements arrive. 
I'm used to cache values to be used on the UI.

I can be reseted (sending #reset), but responsibility of reset caches belongs to presenters who use them.
"
Class {
	#name : #IceTipCache,
	#superclass : #Object,
	#instVars : [
		'proxy',
		'objects'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #'instance creation' }
IceTipCache class >> proxy: aProxy [ 
	^ self new proxy: aProxy 
]

{ #category : #accessing }
IceTipCache >> at: aKey ifAbsent: anObject [
	^ objects 
		at: aKey 
		ifAbsent: anObject
]

{ #category : #accessing }
IceTipCache >> at: aKey ifAbsentPut: anObject [
	^ objects 
		at: aKey 
		ifAbsentPut: anObject
]

{ #category : #accessing }
IceTipCache >> at: aKey ifPresent: anObject [
	^ objects 
		at: aKey 
		ifPresent: anObject
]

{ #category : #accessing }
IceTipCache >> at: aKey put: anObject [
	^ objects 
		at: aKey 
		put: anObject
]

{ #category : #initialization }
IceTipCache >> initialize [
	super initialize.
	self reset
]

{ #category : #accessing }
IceTipCache >> objects [
	^ objects
]

{ #category : #accessing }
IceTipCache >> proxy [
	^ proxy
]

{ #category : #accessing }
IceTipCache >> proxy: anObject [
	proxy := anObject
]

{ #category : #initialization }
IceTipCache >> reset [
	objects := SmallDictionary new
]
