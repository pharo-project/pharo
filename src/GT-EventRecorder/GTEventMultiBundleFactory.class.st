"
I know how to create GTEventMultiBundle object from a collection of GTEventBundle objects.
"
Class {
	#name : #GTEventMultiBundleFactory,
	#superclass : #Object,
	#instVars : [
		'multiBundles'
	],
	#category : #'GT-EventRecorder-Support'
}

{ #category : #accessing }
GTEventMultiBundleFactory class >> default [
	^ self new
]

{ #category : #public }
GTEventMultiBundleFactory class >> fromBundles: aCollection [ 
	^ self default fromBundles: aCollection
]

{ #category : #public }
GTEventMultiBundleFactory >> fromBundles: aCollection [ 
	multiBundles := OrderedDictionary new.
	aCollection do: [ :aSingleOrMultiBundle | self synthetize: aSingleOrMultiBundle.].
	^ self multiBundles
]

{ #category : #accessing }
GTEventMultiBundleFactory >> multiBundles [
	^ multiBundles asOrderedCollection
]

{ #category : #private }
GTEventMultiBundleFactory >> synthetize: aSingleOrMultiBundle [
	aSingleOrMultiBundle accept: self
]

{ #category : #visitor }
GTEventMultiBundleFactory >> visitMultiBundle: aMultiBundle [
	aMultiBundle bundles do: [ :eachSingleBundle |
		self visitSingleBundle: eachSingleBundle ]
]

{ #category : #visitor }
GTEventMultiBundleFactory >> visitSingleBundle: aSingleBundle [
	| multiBundle |
	multiBundle := multiBundles 
							at: aSingleBundle url 
							ifAbsentPut: [ GTEventMultiBundle new url: aSingleBundle url; yourself ].
	multiBundle add: aSingleBundle
]
