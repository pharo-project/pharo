Class {
	#name : 'DeepCopierTestData',
	#superclass : 'Object',
	#instVars : [
		'refer'
	],
	#category : 'DeepCopier',
	#package : 'DeepCopier'
}

{ #category : 'samples' }
DeepCopierTestData class >> aPointsToBThatPointsToA [

	| a b |
	a := self new. 
	b := self new.
	a refer: b.
	b refer: a.
	^ a
]

{ #category : 'samples' }
DeepCopierTestData class >> tripleCircle [

	| a b c |
	a := self new. 
	b := self new.
	c := self new. 
	a refer: b.
	b refer: c.
	c refer: a.
	^ a
]

{ #category : 'accessing' }
DeepCopierTestData >> refer [

	^ refer 
]

{ #category : 'accessing' }
DeepCopierTestData >> refer: anObject [

	refer := anObject 
]
