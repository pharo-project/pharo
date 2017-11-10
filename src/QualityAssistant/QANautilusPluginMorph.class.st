"
I am a morph the plugin displays. I constist of list of critics and a refresh button
"
Class {
	#name : #QANautilusPluginMorph,
	#superclass : #Morph,
	#instVars : [
		'listMorph',
		'progressMorph',
		'entity',
		'pluginDelegate',
		'updateTask',
		'critiques'
	],
	#classInstVars : [
		'wasOpenedBefore'
	],
	#category : #QualityAssistant
}

{ #category : #cleanup }
QANautilusPluginMorph class >> cleanUp [

	wasOpenedBefore := nil.
]

{ #category : #accessing }
QANautilusPluginMorph class >> wasOpenedBefore [

	^ wasOpenedBefore ifNil: [ wasOpenedBefore := false ]
]

{ #category : #accessing }
QANautilusPluginMorph class >> wasOpenedBefore: aBoolean [

	wasOpenedBefore := aBoolean 
]

{ #category : #accessing }
QANautilusPluginMorph >> adoptPaneColor: paneColor [
	super adoptPaneColor: paneColor.
	
	"I have no idea how to solve this in a different way.
	 The list border color is changed throught this method,
	 I have to set progress morphs border to be the same"
	progressMorph borderStyle:
		(Smalltalk ui theme listNormalBorderStyleFor: listMorph).
]

{ #category : #list-morph }
QANautilusPluginMorph >> allowToSelect [
	^ true
]

{ #category : #'announcement handling' }
QANautilusPluginMorph >> classCommented: anAnnouncement [ 

	anAnnouncement classCommented == entity ifTrue: [ 
		self updateList ]
]

{ #category : #list-morph }
QANautilusPluginMorph >> displayCritique: aCritique [
	| listItem |
	listItem := Morph new
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		cellPositioning: #center;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		cellInset: 5;
		color: Color transparent;
		layoutInset:
			(Margin
				left: 0
				top: 0
				right: 10
				bottom: 0);
		yourself.
	listItem addMorph: aCritique title asStringMorph.
	aCritique actions
		do: [ :action | 
			listItem
				addMorphBack:
					(IconicButton new
						target: action;
						labelGraphic: action icon;
						actionSelector: #actOnCritic:ofEntity:;
						arguments:
							{aCritique.
							entity};
						color: Color transparent;
						helpText: action description;
						extent: 16 @ 16;
						borderWidth: 0;
						yourself) ].
	aCritique isCritique
		ifTrue: [ listItem
				addMorphBack:
					(Morph new
						color: Color transparent;
						hResizing: #spaceFill;
						height: 5;
						yourself).
			listItem addMorphBack: (QAFeedbackMorph new critique: aCritique) ].
	^ listItem
]

{ #category : #accessing }
QANautilusPluginMorph >> entity [
	^ entity
]

{ #category : #accessing }
QANautilusPluginMorph >> entity: anEntity [

	entity := anEntity.
	self updateList
]

{ #category : #'task handling' }
QANautilusPluginMorph >> excecuteTask [
	
	self stopTask; setTaskAction.
	updateTask execute.
]

{ #category : #list-morph }
QANautilusPluginMorph >> getCritiques [

	^ critiques
]

{ #category : #list-morph }
QANautilusPluginMorph >> iconFor: item at: anIndex [
	^  item icon
]

{ #category : #list-morph }
QANautilusPluginMorph >> iconMaxSize [

	^ 16 @ 16
]

{ #category : #initialization }
QANautilusPluginMorph >> initialize [
	super initialize.
	
	critiques := #().
	
	self
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		cellPositioning: #center;
		hResizing: #shrinkWrap;
		vResizing: #rigid;
		height: 51; "because it fits 3 row list without showing scrollbars"
		cellInset: 3;
		color: Color transparent.
	
	self initializeList.
	self initializeProgress.
	self initializeUpdateTask.
	
	self addMorphBack: listMorph.
	
	self subscribe
]

{ #category : #initialization }
QANautilusPluginMorph >> initializeList [
	listMorph := NonfocusablePluggableIconListMorph new
		model: self;
		getListSelector: #getCritiques;
		setIndexSelector:  #selected:;
		wrapSelector: #displayCritique:;
		hideHScrollBarIndefinitely: true;
		getIconSelector: #iconFor:at:;
		vResizing: #spaceFill; 
		hResizing: #spaceFill;
		color: self theme backgroundColor;
		backgroundColoringBlockOrSelector: [ :item :index |
			index odd
				ifTrue:  [ Smalltalk ui theme backgroundColor ]
				ifFalse: [ Smalltalk ui theme lightBackgroundColor ] ];
		yourself.
		
]

{ #category : #initialization }
QANautilusPluginMorph >> initializeProgress [

	progressMorph := BorderedMorph new
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		wrapCentering: #center;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		cellInset: 5;
		layoutInset: 30@0;
		color: Smalltalk ui theme backgroundColor;
		yourself.
	
	progressMorph
		addMorph: ('Updating critiques...' asMorph
			color: Smalltalk ui theme disabledTextColor;
			yourself)
	 
]

{ #category : #initialization }
QANautilusPluginMorph >> initializeUpdateTask [

	updateTask := GLMAsyncTask new
		priority: Processor userBackgroundPriority;
		yourself.
]

{ #category : #initialize }
QANautilusPluginMorph >> intoWorld: aWorld [

	updateTask isTerminated ifTrue: [ self setTaskAction ].

	super intoWorld: aWorld
]

{ #category : #'announcement handling' }
QANautilusPluginMorph >> methodModified: anAnnouncement [ 

	anAnnouncement method == entity ifTrue: [ 
		self updateList ]
]

{ #category : #initialize }
QANautilusPluginMorph >> outOfWorld: aWorld [

	self stopTask.
	
	super outOfWorld: aWorld.
]

{ #category : #accessing }
QANautilusPluginMorph >> pluginDelegate [
	^ pluginDelegate
]

{ #category : #accessing }
QANautilusPluginMorph >> pluginDelegate: anObject [
	pluginDelegate := anObject
]

{ #category : #list-morph }
QANautilusPluginMorph >> selected: index [
	| critique |

	(index == 0 or:
	[ pluginDelegate isNil ]) ifTrue: [ ^ self ].
	
	critique := (critiques at: index).
	pluginDelegate critiqueSelected: critique.
	ReSystemAnnouncer uniqueInstance
		notifyCritique: critique
		clickedOnFor: entity.
]

{ #category : #'task handling' }
QANautilusPluginMorph >> setTaskAction [

	updateTask
		doInBackground: [
			critiques := entity
				ifNil: [ #() ]
				ifNotNil: [
					entity externalProperties asArray sort: [ :a :b | a title < b title ] ] ];
		onCompleted: [ self taskCompleted ]
]

{ #category : #'task handling' }
QANautilusPluginMorph >> stopTask [

	updateTask ifNotNil: [ 
		updateTask 
  		terminate;
  		doInBackground: nil;
 		onCompleted: nil
	]
]

{ #category : #initialization }
QANautilusPluginMorph >> subscribe [

	SystemAnnouncer uniqueInstance weak
		when: ClassCommented send: #classCommented: to: self;
		when: MethodRecategorized send: #methodModified: to: self.
]

{ #category : #'task handling' }
QANautilusPluginMorph >> taskCompleted [

	listMorph
	updateList;
	deselectAll.
		
	self removeMorph: progressMorph.
	self addMorphFront: listMorph.
	
	entity ifNotNil: [ ReSystemAnnouncer uniqueInstance
								notifyCritiquesSeen: critiques
								of: entity ].
]

{ #category : #actions }
QANautilusPluginMorph >> updateList [

	self removeAllMorphs.
	self addMorphFront: progressMorph.
	self excecuteTask
]
