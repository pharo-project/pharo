"
I build a tree with information: 
-percent of covered methods in the selected packages
-List of the uncovered methods
I am used in DrTestsUI to show the results in a orderly manner.
"
Class {
	#name : #DTTestCoverageResult,
	#superclass : #DTPluginResult,
	#instVars : [
		'methodList',
		'percent'
	],
	#category : #'DrTests-TestCoverage'
}

{ #category : #accessing }
DTTestCoverageResult >> buildTreeForUI [
	<dtTestCoverageResultTreeNamed: 'List of uncovered methods' order: 1>
	^ DTTreeNode new
		subResults:
			{(DTTreeNode new
				name: (percent * 100 printShowingDecimalPlaces: 2) , ' % Code Coverage';
				subResults: {};
				yourself).
			(DTTreeNode new
				name: 'Uncovered methods';
				subResults: (self methodList collect: #asResultForDrTest);
				yourself)}
]

{ #category : #accessing }
DTTestCoverageResult >> methodList [
	^methodList
]

{ #category : #accessing }
DTTestCoverageResult >> methodList: aCollectionOfMethods [
	methodList := aCollectionOfMethods
]

{ #category : #accessing }
DTTestCoverageResult >> percent [
	^ percent
]

{ #category : #accessing }
DTTestCoverageResult >> percent: anObject [
	percent := anObject
]

{ #category : #accessing }
DTTestCoverageResult >> summarizeInfo [
	"Text showed in miniDrTests with info of the result "

	^ String
		streamContents: [ :s | 
			s
				<< (percent*100 printShowingDecimalPlaces: 2);
				<< ' % Code Coverage';
				<< String cr;
				<< 'Uncovered methods:';
				<< String cr;
				<< self methodList size asString ]
]
