"
I build a tree with the testsResult listed in groups: 
-Errors
-Failures
-Skipped tests
-Passed test
I am used in DrTestsUI to show the results in a orderly manner.
"
Class {
	#name : #DTTestsRunnerResult,
	#superclass : #DTPluginResult,
	#instVars : [
		'testsResult'
	],
	#category : #'DrTests-TestsRunner'
}

{ #category : #'tree building' }
DTTestsRunnerResult >> buildLeavesFrom: aCollection type: testResultType [
	^ aCollection collect: [ :t | DTTestLeaf content: t type: testResultType ]
]

{ #category : #'tree building' }
DTTestsRunnerResult >> buildNodeGroupedByTypeAndClass: anOrderedColletion type: testResultType [
	^ DTTreeNode new
			name: testResultType pluralName;
			subResults: ((anOrderedColletion groupedBy: #class) associations collect: [ :assoc |
				DTTreeNode new
					name: assoc key name;
					subResults: (self buildLeavesFrom: assoc value type: testResultType);
					yourself ]);
			yourself
]

{ #category : #'tree building' }
DTTestsRunnerResult >> buildNodeGroupedByTypeClassAndProtocol: anOrderedColletion type: testResultType [
	^ DTTreeNode new
			name: testResultType pluralName;
			subResults: ((anOrderedColletion groupedBy: #class) associations collect: [ :assoc |
				DTTreeNode new
					name: assoc key name;
					subResults: ((assoc value groupedBy: [ :testCase | (testCase class lookupSelector: testCase selector) protocol ]) associations collect: [ :protocolToTest |
						DTTreeNode new
							name: protocolToTest key;
							subResults: (self buildLeavesFrom: protocolToTest value type: testResultType) ]);
					yourself ]);
			yourself
]

{ #category : #accessing }
DTTestsRunnerResult >> buildTreeForUI [
	<dTTestRunnerResultTreeNamed: 'Grouped by type of result'>
	^ DTTreeNode new
		subResults:
			{DTTreeNode new
				name: DTTestResultType error pluralName;
				subResults: (self buildLeavesFrom: self testsResult errors asOrderedCollection type: DTTestResultType error);
				yourself.
			DTTreeNode new
				name: DTTestResultType fail pluralName;
				subResults: (self buildLeavesFrom: self testsResult failures asOrderedCollection type: DTTestResultType fail).
			DTTreeNode new
				name: DTTestResultType skipped pluralName;
				subResults: (self buildLeavesFrom: self testsResult skipped asOrderedCollection type: DTTestResultType skipped).
			DTTreeNode new
				name: DTTestResultType pass pluralName;
				subResults: (self buildLeavesFrom: self testsResult passed asOrderedCollection type: DTTestResultType pass)
		};
		yourself
]

{ #category : #accessing }
DTTestsRunnerResult >> buildTreeForUIByClasses [
	<dTTestRunnerResultTreeNamed: 'Grouped by type of result and classes'>

	| errors failures skipped passed |
	errors := self buildNodeGroupedByTypeAndClass: self testsResult errors type: DTTestResultType error.
	failures := self buildNodeGroupedByTypeAndClass: self testsResult failures asOrderedCollection type: DTTestResultType fail.
	skipped := self buildNodeGroupedByTypeAndClass: self testsResult skipped type: DTTestResultType skipped.
	passed := self buildNodeGroupedByTypeAndClass: self testsResult passed type: DTTestResultType pass.
	^ DTTreeNode new
		subResults:
			{errors.
			failures.
			skipped.
			passed};
		yourself
]

{ #category : #accessing }
DTTestsRunnerResult >> buildTreeForUIByClassesAndProtocol [
	<dTTestRunnerResultTreeNamed: 'Grouped by type of result, classes, and protocol'>

	| errors failures skipped passed |
	errors := self buildNodeGroupedByTypeClassAndProtocol: self testsResult errors type: DTTestResultType error.
	failures := self buildNodeGroupedByTypeClassAndProtocol: self testsResult failures asOrderedCollection type: DTTestResultType fail.
	skipped := self buildNodeGroupedByTypeClassAndProtocol: self testsResult skipped type: DTTestResultType skipped.
	passed := self buildNodeGroupedByTypeClassAndProtocol: self testsResult passed type: DTTestResultType pass.
	^ DTTreeNode new
		subResults:
			{errors.
			failures.
			skipped.
			passed};
		yourself
]

{ #category : #accessing }
DTTestsRunnerResult >> color [
	testsResult errors ifNotEmpty: [ ^ self theme dangerBackgroundColor ].
	testsResult failures ifNotEmpty: [ ^ self theme warningBackgroundColor ].
	^ self theme successBackgroundColor
]

{ #category : #accessing }
DTTestsRunnerResult >> summarizeInfo [
	"Text showed in miniDrTests with info of the result "

	^ String
		streamContents: [ :s | 
			s
				print: self testsResult passed size;
				<< ' passed';
				cr;
				print: self testsResult failures size;
				<< ' failures';
				cr;
				print: self testsResult errors size;
				<< ' errors';
				cr;
				print: self testsResult skipped size;
				<< ' skipped' ]
]

{ #category : #accessing }
DTTestsRunnerResult >> testsResult [
	^ testsResult
]

{ #category : #accessing }
DTTestsRunnerResult >> testsResult: anObject [
	testsResult := anObject
]
