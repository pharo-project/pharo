"
I am a mini UI TestRunner and i present the result as a Tree.
"
Class {
	#name : 'TestRunnerTreePresenter',
	#superclass : 'TestRunnerAbstractPresenter',
	#category : 'TestRunner-UI',
	#package : 'TestRunner-UI'
}

{ #category : 'instance creation' }
TestRunnerTreePresenter class >> open [ 
	<script>
	
	^ self new open
]

{ #category : 'presenters-initialization' }
TestRunnerTreePresenter >> childreenFor: aParent [

	aParent = #failures ifTrue: [ ^ runner result failures ].
	aParent = #errors ifTrue: [ ^ runner result errors ].
	"..."
	^ {  }
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerTreePresenter >> displayTestCases [

	self generateTree
]

{ #category : 'presenters-initialization' }
TestRunnerTreePresenter >> generateTree [

	resultItemsPresenter
		roots: self rootCandidates;
		children: [ :parent | 
			(self childreenFor: parent) asOrderedCollection ];
		expandPath: #( 1 )
]

{ #category : 'presenters-initialization' }
TestRunnerTreePresenter >> initializeResultItemsPresenter [

	resultItemsPresenter := self newTreeTable.
	resultItemsPresenter
		addColumn: (SpCompositeTableColumn new
				 title: 'tests';
				 addColumn: (SpStringTableColumn evaluated: #asString);
				 yourself);
		addColumn: (SpCompositeTableColumn new
				 width: 50;
				 title: 'total';
				 yourself);
		contextMenu: self menu.
	self generateTree
]

{ #category : 'presenters-initialization' }
TestRunnerTreePresenter >> menu [

	^ self newMenu
		  title: 'Options';
		  addGroup: [ :aGroup | 
			  aGroup
				  addItem: [ :anItem | 
					  anItem
						  name: 'Run';
						  description: 'Run the selected test';
						  action: [ self runSelectedTest ] ];
				  addItem: [ :anItem | 
					  anItem
						  name: 'Debug';
						  description: 'Debug the selected test';
						  action: [ self debugSelectedTest ] ] ]
]

{ #category : 'presenters-initialization' }
TestRunnerTreePresenter >> rootCandidates [

	| rootCandidats |
	rootCandidats := OrderedCollection new.
	runner someTestFailed ifTrue: [ rootCandidats add: #failures ].
	runner someTestCausesError ifTrue: [ rootCandidats add: #errors ].
	^ rootCandidats
]

{ #category : 'presenters-initialization' }
TestRunnerTreePresenter >> windowName [
	^ 'UIRunner (Tree)'
]
