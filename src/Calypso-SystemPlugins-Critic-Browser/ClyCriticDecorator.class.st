"
I am a browser too decorator which adds extra panel with table of critiques related to the model of tool.
I show critiques of methods, classes and packages.

During tool decoration I create query view (criticView) on ClyAllBasisCritiques query in async mode.
While query is executed the criticView is hidden and special animation indicates analysis process in the status bar of the tool.
When query is complete and include any critique I show criticView panel to the user. 
 
Internal Representation and Key Implementation Points.

    Instance Variables
	analysisScope:		<ClyScope>
	criticView:		<ClyQueryView>
	originalToolPanel:		<Morph>
	progressMorph:		<ClyActivityAnimationIconMorph>
"
Class {
	#name : #ClyCriticDecorator,
	#superclass : #ClyBrowserToolDecorator,
	#instVars : [
		'criticView',
		'originalToolPanel',
		'analysisScope',
		'progressMorph'
	],
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #decoration }
ClyCriticDecorator >> adoptBrowserToolLayout [

	originalToolPanel := PanelMorph new.
	originalToolPanel 
		name: 'original tool elements';
		color: Color transparent;		
		borderWidth: 0;
		layoutPolicy: browserTool layoutPolicy.
	browserTool submorphs do: [ :each | originalToolPanel addMorphBack: each ].
	browserTool changeProportionalLayout.
	browserTool addMorph: originalToolPanel fullFrame: (0@0 corner: 1@1) asLayoutFrame
]

{ #category : #accessing }
ClyCriticDecorator >> analyzedObject [
	^analysisScope basisObjects anyOne
]

{ #category : #decoration }
ClyCriticDecorator >> buildCriticalTextSegmentsFor: anObject [

	| interval segment severity |
	^criticView dataSource allElements
		select: [ :critique | critique hasProperty: ClySourceNodeCritiqueIntervalProperty ]
		thenCollect: [ :critique |
			interval := critique getProperty: ClySourceNodeCritiqueIntervalProperty.		
			segment := ClyCriticalTextSegmentMorph
				from: interval start
				to: interval stop + 1.
			severity := critique getProperty: ClyCritiqueSeverityProperty.
			segment
				label: critique name;
				icon: (browserTool iconNamed: severity iconName);
				color: severity color;
				critique: critique actualObject;
				entity: anObject;
				yourself]
]

{ #category : #controlling }
ClyCriticDecorator >> changeStateBy: aBlock [
	aBlock value

]

{ #category : #controlling }
ClyCriticDecorator >> changeStateOf: aQueryView by: aBlock [
	aBlock value

]

{ #category : #decoration }
ClyCriticDecorator >> createCriticView [

	criticView := ClyQueryViewMorph for: self.
	criticView 
		name: 'critiques table';
		hResizing: #spaceFill;
		vResizing: #rigid;
		borderWidth: 4;
		borderColor: browserTool containerTab backgroundColor.
	criticView height: 50
]

{ #category : #accessing }
ClyCriticDecorator >> criticView [
	^criticView
]

{ #category : #decoration }
ClyCriticDecorator >> decorateTool [
	| newScope |	
	criticView ifNil: [ self prepare].	
	newScope := browserTool criticAnalysisScope.
	(analysisScope isNil or: [ newScope basisObjects ~= analysisScope basisObjects]) ifTrue: [ 
		analysisScope := newScope.
		self updateCriticView].
	
	self refreshCritiques
]

{ #category : #controlling }
ClyCriticDecorator >> hideCritiques [
 
	criticView delete.
	browserTool removePaneSplitters.
	originalToolPanel layoutFrame bottomFraction: 1; bottomOffset: 0.
	
]

{ #category : #controlling }
ClyCriticDecorator >> ignoreNavigationDuring: aBlock [ 
	aBlock value
]

{ #category : #controlling }
ClyCriticDecorator >> itemsChanged [
	self refreshCritiques
]

{ #category : #decoration }
ClyCriticDecorator >> prepare [
	self adoptBrowserToolLayout.
	self createCriticView.
	progressMorph := ClyActivityAnimationIconMorph label: 'critiques analysis'
]

{ #category : #controlling }
ClyCriticDecorator >> refreshCritiques [
	browserTool decorateByCritic: self.
	
	criticView dataSource isEmpty 
		ifTrue: [ self hideCritiques ]
		ifFalse: [ self showCritiques ].
	(criticView dataSource hasMetaProperty: ClyBackgroundProcessingTag)
		ifFalse: [ progressMorph delete. browserTool statusBar update ]
		ifTrue: [ browserTool statusBar addContextItem: progressMorph  ]
]

{ #category : #controlling }
ClyCriticDecorator >> showCritiques [
	criticView owner ifNotNil: [ ^self ].
	
	originalToolPanel layoutFrame bottomFraction: 0.8.
	browserTool addMorph: criticView fullFrame: ((0@0.8 corner: 1@1) asLayoutFrame).
	browserTool addPaneSplitters
]

{ #category : #controlling }
ClyCriticDecorator >> toolWasRemoved [
	
	criticView ifNotNil: [ criticView closeDataSource ]
]

{ #category : #controlling }
ClyCriticDecorator >> updateCriticView [
	
	| critiqueQuery |
	
	critiqueQuery := (ClyAllBasisCritiqueQuery from: analysisScope) async.
	criticView showQuery: critiqueQuery
]
