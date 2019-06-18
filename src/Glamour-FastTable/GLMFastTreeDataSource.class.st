"
I'm a model datasource to handle glamour fast tree presentations.

Description 
--------------------

My basics functionnalities works as in my superclass but I do not get my data at my creation. Instead I ask to my Glamour presentation to get them.

I work with a GLMFastTreePresentation and a GLMMorphicFastTreeRenderer. 

Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	announcer:		This is an announcer use to work with Glamour workflow. 
	glamourPresentation:		This is a glamour presentation use to get some informations as the data manage by myself.
	tagsToFilterBy:		This is a list of filtering tags for the dasaSource.


"
Class {
	#name : #GLMFastTreeDataSource,
	#superclass : #FTTreeDataSource,
	#traits : 'TGLMFastTable + TGLMFastTableTag',
	#classTraits : 'TGLMFastTable classTrait + TGLMFastTableTag classTrait',
	#instVars : [
		'glamourPresentation',
		'announcer',
		'tagsToFilterBy',
		'dataCache'
	],
	#category : #'Glamour-FastTable-Model'
}

{ #category : #accessing }
GLMFastTreeDataSource >> announcer [
	^ announcer ifNil: [ announcer := GLMAnnouncer new ]
]

{ #category : #private }
GLMFastTreeDataSource >> basicRoots [
	^ self glamourPresentation displayValue
]

{ #category : #accessing }
GLMFastTreeDataSource >> buttonFor: item [
	^ ( (self canDisplayChildrenOf: item) and: [self hasChildren: item])
		ifTrue: [ item generateButton ]
		ifFalse: [ self class emptyMorph ]
]

{ #category : #accessing }
GLMFastTreeDataSource >> cellColumn: aColumnIndex row: aRowIndex [
	| item cell |
	item := self elementAt: aRowIndex.
	cell := FTCellMorph new
		cellInset: 5;
		addMorph: (self rowMorphForItem: item);
		yourself.
	^ cell
]

{ #category : #private }
GLMFastTreeDataSource >> createRootItem [
	| fullRoot |
	fullRoot := FTRootItem data: self basicRoots from: self.
	^ self tagsToFilterBy
		ifEmpty: [ fullRoot ]
		ifNotEmpty:
			[ | children |
			children := OrderedCollection new.
			fullRoot children
				do: [ :item | (self matchingTag: item) ifNotNil: [ :itemNew | children add: itemNew ] ]
				displayingProgress: [ :each | 'Looking inside ' , each printString ].
			FTRootItem new
				dataSource: self;
				children: children asArray;
				yourself ]
]

{ #category : #cache }
GLMFastTreeDataSource >> dataCache [
	^ dataCache ifNil: [ dataCache := self class defaultCache ]
]

{ #category : #selecting }
GLMFastTreeDataSource >> explicitSelection: anObject [
	self table selectIndexes: (self searchText: anObject asString)
]

{ #category : #accessing }
GLMFastTreeDataSource >> glamourPresentation [
	^ glamourPresentation
]

{ #category : #accessing }
GLMFastTreeDataSource >> glamourPresentation: aGLMFastTreePresentation [
	glamourPresentation := aGLMFastTreePresentation
]

{ #category : #testing }
GLMFastTreeDataSource >> hasChildren: anItem [
	^ self glamourPresentation hasChildrenBlock 
		ifNil: [ anItem children isNotEmpty ]
		ifNotNil: [ :aBlock | aBlock cull: anItem data ]
]

{ #category : #tag }
GLMFastTreeDataSource >> matchingTag: anItem [
	| elementTags |
	elementTags := self glamourPresentation tagsFor: anItem data to: #filter.
	((anItem children collect: [ :it | self matchingTag: it ]) reject: #isNil)
		ifNotEmpty: [ :coll | 
			^ (self newStaticItemFrom: anItem)
				children: coll asArray;
				yourself ].
	^ (self tagsToFilterBy allSatisfy: [ :each | elementTags includes: each ])
		ifFalse: [ nil ]
		ifTrue: [ self newStaticItemFrom: anItem ]
]

{ #category : #'instance creation' }
GLMFastTreeDataSource >> newStaticItemFrom: anItem [
	^ FTStaticBasicItem new
		data: anItem data;
		depth: anItem depth;
		children: #();
		dataSource: self;
		yourself
]

{ #category : #private }
GLMFastTreeDataSource >> resetChildrenContent [
	self rootItem: self createRootItem.
	self resetDataCache
]

{ #category : #cache }
GLMFastTreeDataSource >> resetDataCache [
	dataCache := nil
]

{ #category : #accessing }
GLMFastTreeDataSource >> resetElements [
	self resetDataCache.
	self updateData
]

{ #category : #accessing }
GLMFastTreeDataSource >> rootItem [
	^ rootItem ifNil: [ rootItem := self createRootItem ]
]

{ #category : #private }
GLMFastTreeDataSource >> rowMorphForItem: anItem [
	| rowElements |
	rowElements := OrderedCollection new.
	anItem depth timesRepeat: [ rowElements add: self class emptyMorph ].
	rowElements add: (self buttonFor: anItem).
	(self iconFor: anItem data) ifNotNil: [ :icon | rowElements add: icon asMorph ].
	rowElements
		add: (self formatedDisplayValueOf: anItem data) asMorph asReadOnlyMorph;
		addAll: (self tagMorphsFrom: self glamourPresentation for: anItem data).
	^ Smalltalk ui theme newRowIn: self currentWorld for: rowElements
]

{ #category : #selecting }
GLMFastTreeDataSource >> selection: ann [
	| selection |
	selection := ann newSelectedIndexes
		select: [ :each | each ~= 0 ]
		thenCollect: [ :each | (self elementAt: each) data ].
	selection
		ifNotEmpty: [ self announcer
				announce:
					(GLMTreeMorphSelectionChanged new
						selectionValue:
							((selection size = 1 and: [ self glamourPresentation isSingle ])
								ifTrue: [ selection first ]
								ifFalse: [ selection ]);
						selectionPathValue: selection;
						yourself) ]
]

{ #category : #selecting }
GLMFastTreeDataSource >> strongSelection: ann [
	self announcer
		announce:
			(GLMTreeMorphStrongSelectionChanged new
				strongSelectionValue: (self elementAt: ann selectedIndex) data;
				yourself)
]

{ #category : #accessing }
GLMFastTreeDataSource >> tagsToFilterBy [
	^ tagsToFilterBy ifNil: [ tagsToFilterBy := OrderedCollection new ]
]
