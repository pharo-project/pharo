Class {
	#name : #IceTipBranchWithCommitStatusBarItem,
	#superclass : #IceTipStatusBarItem,
	#instVars : [
		'model'
	],
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #factory }
IceTipBranchWithCommitStatusBarItem >> branchNameMorph [
	^ ('{1} at ' format: {model branchName}) asMorph
]

{ #category : #accessing }
IceTipBranchWithCommitStatusBarItem >> model [
	^ model
]

{ #category : #accessing }
IceTipBranchWithCommitStatusBarItem >> model: anObject [
	model := anObject
]

{ #category : #factory }
IceTipBranchWithCommitStatusBarItem >> newStatusBarItemOn: aStatusBar [
	self model shortCommitId ifNil: [ ^ self model branchName asMorph ].

	^ PanelMorph new
		addMorph: self branchNameMorph;
		addMorph: self shortIdMorph;
		changeTableLayout;
		listDirection: #rightToLeft;
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		yourself
]

{ #category : #factory }
IceTipBranchWithCommitStatusBarItem >> shortIdMorph [
	^ (IceTipURLLabelMorph contents: model shortCommitId)
		setBalloonText: 'Copy the commit ID to the clipboard';
		onClick: [ self model copyCommitIDToClipboard ];
		yourself
	
]
