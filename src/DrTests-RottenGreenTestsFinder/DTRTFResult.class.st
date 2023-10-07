Class {
	#name : 'DTRTFResult',
	#superclass : 'DTPluginResult',
	#instVars : [
		'result'
	],
	#category : 'DrTests-RottenGreenTestsFinder-Base',
	#package : 'DrTests-RottenGreenTestsFinder',
	#tag : 'Base'
}

{ #category : 'adding' }
DTRTFResult >> add: aRottenTestsSet [

	self result mergeWith: aRottenTestsSet
]

{ #category : 'accessing' }
DTRTFResult >> addRottenTest: aRottenTest [

	self rottenTests add: aRottenTest
]

{ #category : 'accessing' }
DTRTFResult >> buildTreeForUI [
	"build the UI for presenting result"
	<dtTestRTFResultTreeNamed: 'List of rotten green tests' order: 1>
	^ DTTreeNode new subResults: { (DTTreeNode new
			   name: 'Rotten green tests';
			   subResults:
				   (self rottenTests collect: [ :m | m compiledMethod  asResultForDrTest ]);
			   yourself) }
]

{ #category : 'accessing' }
DTRTFResult >> result [

	^ result ifNil: [ result := RottenTestsSet new ]
]

{ #category : 'accessing' }
DTRTFResult >> rottenTests [

	^ (self result rottenTests ifNil: [ result := RottenTestsSet ])
		  asOrderedCollection
]
