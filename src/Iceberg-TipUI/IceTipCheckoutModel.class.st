Class {
	#name : #IceTipCheckoutModel,
	#superclass : #IceTipDiffPreviewModel,
	#instVars : [
		'checkoutStrategy',
		'checkoutStrategies'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #actions }
IceTipCheckoutModel >> checkout [

	checkoutStrategy
		committish: self commitish;
		diff: diffModel entity.

	self commitish checkout: checkoutStrategy
]

{ #category : #accessing }
IceTipCheckoutModel >> checkoutStrategies [
	
	^ checkoutStrategies
]

{ #category : #accessing }
IceTipCheckoutModel >> checkoutStrategy [
	
	^ checkoutStrategy
]

{ #category : #accessing }
IceTipCheckoutModel >> checkoutStrategy: aStrategy [
	checkoutStrategy := aStrategy
]

{ #category : #actions }
IceTipCheckoutModel >> executeCheckoutActionAndThen: aBlock [ 
	
	<noCache>
	IceTipStandardAction new 
		repository: self repository;
		message: ('Checking out {1} from {2}' format: { self targetName. self repositoryName });
		onSuccessRepositoryModified;
		onSuccess: aBlock;
		execute: [ self checkout ]
]

{ #category : #initialization }
IceTipCheckoutModel >> initialize [

	super initialize.
	checkoutStrategies := (IceCheckoutStrategy allSubclasses 
		sorted: [ :a :b | a order < b order ])
		collect: #new.
	checkoutStrategy := checkoutStrategies first.
]
