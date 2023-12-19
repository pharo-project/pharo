"
I am a toolbar used to navigate the results tree of the profiling.
My items are the buttons I contain which can be clicked by users
"
Class {
	#name : 'StProfilerToolbarPresenter',
	#superclass : 'SpToolbarPresenter',
	#instVars : [
		'selectedItem'
	],
	#category : 'ProfilerUI-View',
	#package : 'ProfilerUI',
	#tag : 'View'
}

{ #category : 'adding-removing items' }
StProfilerToolbarPresenter >> ensureItemWith: aTallyModel [

	| newItem |
	(self getItemWith: aTallyModel) ifNotNil: [ :found | ^ found ].

	self selectedItem ifNotNil: [ :selected |
		self removeItemsAfter: selected ].

	newItem := self newItemFrom: aTallyModel.
	self addItem: newItem.
	^ newItem
]

{ #category : 'accessing' }
StProfilerToolbarPresenter >> getItemWith: anObject [

	^ (self items select: [ :item | item object == anObject ])
		  ifEmpty: [ nil ]
		  ifNotEmpty: [ :result | result anyOne ]
]

{ #category : 'factory' }
StProfilerToolbarPresenter >> newItemFrom: aTallyModel [

	| button |
	button := StProfilerToolbarButtonPresenter new.

	button
		object: aTallyModel;
		label: aTallyModel methodName;
		selectedObject: aTallyModel;
		help: 'Display ' , aTallyModel methodName , ' as root';
		action: [
			aTallyModel model setTallyRoot: aTallyModel.
			self setSelectedItem: button
			].

	^ button
]

{ #category : 'adding-removing items' }
StProfilerToolbarPresenter >> removeItemsAfter: anItem [
	| toKeep item i |

	toKeep := #() asOrderedCollection.
	i := 1.
	[
	item := self items at: i.
	toKeep add: item.
	i := i + 1.
	item isNotNil and: [ item = anItem ] ] whileFalse.
	self removeAllItems.
	toKeep do: [ :itm | self addItem: itm ]
]

{ #category : 'reseting' }
StProfilerToolbarPresenter >> reset [
	self selectedItem: nil.
	self removeAllItems
]

{ #category : 'accessing' }
StProfilerToolbarPresenter >> selectedItem [

	^ selectedItem
]

{ #category : 'accessing' }
StProfilerToolbarPresenter >> selectedItem: anObject [

	selectedItem := anObject
]

{ #category : 'accessing' }
StProfilerToolbarPresenter >> setSelectedItem: aToolbarButtonPresenter [

	self items do: [:itm | itm icon: nil ].
	aToolbarButtonPresenter icon: (self iconNamed: 'windowMenu').
	self selectedItem: aToolbarButtonPresenter
]
