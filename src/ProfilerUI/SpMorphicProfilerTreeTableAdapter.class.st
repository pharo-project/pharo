"
I am the adapter used to realise a ProfilerTreeTablePresenter.

I will create a tree table (using FTTableMorph), but my datasource uses ProfilerFTBasicItem and ProfilerFTRoot item, which optimize the performance rendering the tree, but use more memory.
"
Class {
	#name : 'SpMorphicProfilerTreeTableAdapter',
	#superclass : 'SpMorphicTreeTableAdapter',
	#category : 'ProfilerUI-SpecAdapters',
	#package : 'ProfilerUI',
	#tag : 'SpecAdapters'
}

{ #category : 'factory' }
SpMorphicProfilerTreeTableAdapter >> addModelTo: tableMorph [

	self ensureAtLeastOneColumnIn: tableMorph.

	self presenter selection isMultipleSelection ifTrue: [
		tableMorph beMultipleSelection ].

	tableMorph setBalloonText: self model help.

	self presenter selection isEmpty ifFalse: [
		self updateSelectionOf: tableMorph ].

	self presenter whenRootsChangedDo: [ tableMorph buildContents ]
]

{ #category : 'factory' }
SpMorphicProfilerTreeTableAdapter >> buildWidget [
	| tableMorph |

	tableMorph := (MorphTreeMorph on: self presenter viewModel)
		rowInset: 2;
		columnInset: 4;
		autoDeselection: true;
		getMenuSelector:#menu:shifted:;
		keyDownActionSelector: #keyDown:from:;
		treeLineWidth: 1;
		treeLineDashes: {5. 1};
		lineColorBlock: [:node | {Color gray. Color orange. Color brown. Color magenta. Color blue} at: ((node level \\ 5) + 1)];
		doubleClickSelector: #browseItem;
		rowColorForEven: self theme lightBackgroundColor;
		yourself.

	self addModelTo: tableMorph.
	tableMorph buildContents.

	widget := tableMorph.

	^ tableMorph
]

{ #category : 'accessing' }
SpMorphicProfilerTreeTableAdapter >> collapseAll [
	^ widget collapseAll
]

{ #category : 'accessing' }
SpMorphicProfilerTreeTableAdapter >> collapsePath: anArray [

	widget collapseNodePath: anArray
]

{ #category : 'accessing' }
SpMorphicProfilerTreeTableAdapter >> expandAll [
	^ widget expandAll
]

{ #category : 'accessing' }
SpMorphicProfilerTreeTableAdapter >> expandPath: anArray [

	widget expandNodePath: anArray
]

{ #category : 'accessing' }
SpMorphicProfilerTreeTableAdapter >> expandRoots [

	widget expandRoots
]

{ #category : 'updating' }
SpMorphicProfilerTreeTableAdapter >> refreshTree [

	widget updateList
]

{ #category : 'accessing' }
SpMorphicProfilerTreeTableAdapter >> scrollToSelection [
	self widget scrollSelectionIntoView
]

{ #category : 'accessing' }
SpMorphicProfilerTreeTableAdapter >> selectedItem [

	^ self widget selectedMorph complexContents
]
