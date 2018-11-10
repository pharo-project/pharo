"
I preview the changes that will be introduced in the image when a merge occurs.

(self onBranch: IceRepository registry anyOne branch) openWithSpec.
"
Class {
	#name : #IceTipMergePreviewBrowser,
	#superclass : #IceTipPreviewBrowser,
	#instVars : [
		'mergeType'
	],
	#category : #'Iceberg-TipUI-View-Branch'
}

{ #category : #'instance creation' }
IceTipMergePreviewBrowser class >> onBranch: aBranch [

	^ self on: (IceTipMergeModel new
		commitish: aBranch;
		yourself)
]

{ #category : #actions }
IceTipMergePreviewBrowser >> doMerge [

	self model executeMergeActionAndThen: [ self window delete ]
]

{ #category : #initialization }
IceTipMergePreviewBrowser >> initializeWidgets [ 
	super initializeWidgets.
	diffPanel leftLabel: 'Working Copy'.
	diffPanel rightLabel: 'Merge with ' , model targetName.

]

{ #category : #accessing }
IceTipMergePreviewBrowser >> mergeType: anObject [
	
	self model mergeType: anObject
]

{ #category : #'private factory' }
IceTipMergePreviewBrowser >> newActionButton [
	^ self newButton 
		label: 'Merge';
		icon: (self iconNamed: #merge);
		help: ('Merges {1} with your image''s version.' format: { self model targetName });
		action: [ self doMerge ];
		yourself
]

{ #category : #api }
IceTipMergePreviewBrowser >> title [
	^ 'Preview merge of {1}/{2}' format: { 
		self model repositoryName.
		self model targetName }
]
