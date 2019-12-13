"
I consolidate GTEventCollector objects into one or more collectors according to their URL and category. That way we can pack together data, that belongs to the same application (= URL and category) and we can save them together on the server side as one bundle.

For the Collaborators Part: 
- GTEventCollector objects that I consolidate
- GTEventRecorder that uses me before packing process.

Public API and Key Messages

- collectors: set a collection of collectors that should be consolidated
- consolidated returns collection of GTEventCollector objects.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	collectors:		<OrderedCollection>

"
Class {
	#name : #GTEventConsolidate,
	#superclass : #Object,
	#instVars : [
		'collectors'
	],
	#category : #'GT-EventRecorder-Support'
}

{ #category : #accessing }
GTEventConsolidate >> collectors: aCollection [ 
	collectors := aCollection
]

{ #category : #'accessing - computed' }
GTEventConsolidate >> consolidated [
	| consolidatedCollectors |
	consolidatedCollectors := OrderedDictionary new.
	collectors do: [ :eachCollector | | aCollector |
		aCollector := consolidatedCollectors 
								at: (eachCollector url printString, '/', eachCollector category) asSymbol
								ifAbsentPut: [ GTEventCollector new 
														url: eachCollector url; 
														category: eachCollector category;
														yourself ].
		eachCollector bundle do: [ :eachEvent | aCollector add: eachEvent ] ].
	^ consolidatedCollectors asArray
]
