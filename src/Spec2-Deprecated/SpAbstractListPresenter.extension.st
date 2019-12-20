Extension { #name : #SpAbstractListPresenter }

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> doubleClickAction: aBlockClosure [ 
	
	self
		deprecated: 'Use whenActivatedDo: instead'
		transformWith: '`@receiver whenActivatedDo: `@argument'
						-> '`@receiver doubleClickAction: `@argument'.
	
	self whenActivatedDo: [ :sel | aBlockClosure value: sel selectedItem]
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> filteringBlock [
	self
		deprecated: 'This feature is now removed from Spec 2. If the visible elements of the list need to be changed, it''s the users of the lists that should manage it and update the list of items of the list.'.
	^ nil
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> filteringBlock: aBlock [
	self
		deprecated: 'This feature is now removed from Spec 2. If the visible elements of the list need to be changed, it''s the users of the lists that should manage it and update the list of items of the list.'
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> getSelectionStateFor: anIndex [
	self deprecated: 'This method from the old API will be removed.' transformWith: '`@receiver getSelectionStateFor: `@statements' -> '`@receiver selection selectedIndexes includes: `@statements'.

	^ self selection selectedIndexes includes: anIndex
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> itemBeginsWithFilter [
	self deprecated: 'Use #enableItemBeginsWithFilter instead' transformWith: '`@receiver itemBeginsWithFilter' -> '`@receiver enableItemBeginsWithFilter'.
	self enableItemBeginsWithFilter
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> itemFilterBlock [
	self deprecated: 'Use #itemFilter instead' transformWith: '`@receiver itemFilterBlock' -> '`@receiver itemFilter'.
	^ self itemFilter
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> itemFilterBlock: aBlock [
	self deprecated: 'Use #itemFilter: instead' transformWith: '`@receiver itemFilterBlock: `@statements1' -> '`@receiver itemFilter: `@statements1'.
	^ self itemFilter: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> itemSubstringFilter [
	self deprecated: 'Use #enableItemSubstringFilter instead' transformWith: '`@receiver itemSubstringFilter' -> '`@receiver enableItemSubstringFilter'.
	self enableItemSubstringFilter
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> listItems [
	
	self
		deprecated: 'Please use the #model instead'
		transformWith: '`@receiver listItems' 
						-> '`@receiver model items'.	
	^ self model items
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> menu [
	"Return the block used to defined the menu"
	self 
		deprecated: 'Use #contextMenu instead.' 
		transformWith: '`@receiver menu' 
						-> '`@receiver contextMenu'.
	
	^ self contextMenu
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> menu: aBlock [
	"Set the block used to defined the menu"

	self
		deprecated:
			'Use #contextMenu: instead. The old way to create a MenuPresenter was this method taking a MenuMorph as parameter. This will not work with other backend and will be totally removed in Pharo7. Now you should use #contextMenu takin a MenuPresenter (or a block returning a menu presenter) as parameter.'
		on: '2019-03-07'
		in: #Pharo8.
	self
		contextMenu: [ aBlock
				cull:
					(MenuMorph new
						defaultTarget: self;
						yourself)
				cull: false ]
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> menu: aBlock shifted: aBoolean [
	self
		deprecated:
			'Use #contextMenu: instead. The old way to create a MenuPresenter was this method taking a MenuMorph as parameter. This will not work with other backend and will be totally removed in Pharo7. Now you should use #contextMenu takin a MenuPresenter (or a block returning a menu presenter) as parameter.'
		on: '2019-03-07'
		in: #Pharo8.
	self
		contextMenu: [ aBlock
				cull:
					(MenuMorph new
						defaultTarget: self;
						yourself)
				cull: true ]
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> resetSelection [
	"Unselect every items"

	self
		deprecated: 'Please use #unselectAll instead'
		transformWith: '`@receiver resetSelection' 
						-> '`@receiver unselectAll'.

	self unselectAll
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> selectedIndex [
	"Return the index of the selected item
	In the case of a multiple selection list, it returns the last selected item"
	
	self
		deprecated: 'Use the #selection object instead. This method assumes single selection'
		transformWith: '`@receiver selectedIndex'
						-> '`@receiver selection selectedIndex'.
	
	^  self selection selectedIndex
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> selectedIndex: anIndex [

	"Set the index of the item you want to be selected"
	self
		deprecated: 'Use #selectIndex: instead'
		transformWith: '`@receiver selectedIndex: `@arg'
						-> '`@receiver selectIndex: `@arg'.
	
	self selectIndex: anIndex
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> selectedIndexes [
	self
		deprecated: 'Use the #selection object instead. This method assumes multiple selection'
		transformWith: '`@receiver selectedIndexes'
						-> '`@receiver selection selectedIndexes'.

	^  self selection selectedIndexes
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> selectedItem [
	"Return the selected item.
	In the case of a multiple selection list, it returns the last selected item"
	
	self
		deprecated: 'Use the #selection object instead. This method assumes single selection'
		transformWith: '`@receiver selectedItem'
						-> '`@receiver selection selectedItem'.
	
	^ self selection selectedItem
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> selectedItem: anItem [
	"Set the item you want to be selected"

	self
		deprecated: 'Use #selectItem: instead'
		transformWith: '`@receiver selectedItem: `@arg'
						-> '`@receiver selectItem: `@arg'.

	self selectItem: anItem
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> setIndex: anIndex [
	self
		deprecated: 'Use #selectIndex: instead'
		transformWith: '`@receiver setIndex: `@arg' -> '`@receiver selectIndex: `@arg'.
	self selectIndex: anIndex
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> setIndexes: aCollectionOfIndexes [
	"Set the indexexs of the selected items in case of multiple selection"
	self
		deprecated: 'Use the #selection object instead. This method assumes multiple selection'
		transformWith: '`@receiver setIndexes: `@arg'
						-> '`@receiver selection selectIndexes: `@arg'.

	self selection selectIndexes: aCollectionOfIndexes
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> setSelectedIndex: anIndex [
	self
		deprecated: 'Use #selectedIndex: instead'
		transformWith:
			'`@receiver setSelectedIndex: `@argument'
				-> '`@receiver selectedIndex: `@argument'.
	^ self selectIndex: anIndex
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> setSelectedItem: anIndex [
	self
		deprecated: 'Use #selectedItem: instead'
		transformWith:
			'`@receiver setSelectedItem: `@argument'
				-> '`@receiver selectedItem:`@argument'.
	^ self selectItem: anIndex
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> whenListChanged: aBlock [
	"Specify a block to value after the contents of the list has changed"
	"Basically when you set a new list of items"
	self
		deprecated: 'Use #whenModelChangedDo: instead'
		transformWith: '`@receiver whenListChanged: `@argument'
						-> '`@receiver whenModelChangedDo: `@argument'.
	
	self whenModelChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> whenMultiSelectionChanged: aBlock [
	"Set a block to value when the multiSelection value has changed"
	self deprecated: 'Should not use'
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> whenSelectedIndexChangedDo: aBlock [
	"Set a block to value when the selection index has changed"
	
	self
		deprecated: 'Use #selection whenChangedDo: instead'
		transformWith: '`@receiver whenSelectedIndexChangedDo: `@argument'
						-> '`@receiver selection whenChangedDo: [ :selection | `@argument value: selection selectedIndex ]'.

	^ self  selection whenChangedDo: [ :selection | aBlock value: selection selectedIndex ]
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> whenSelectedItemChanged: aBlock [
	self
		deprecated: 'Use #whenSelectedItemChangedDo: instead'
		transformWith:
			'`@receiver whenSelectedItemChanged: `@argument'
				-> '`@receiver whenSelectedItemChangedDo: `@argument'.
	^ self
		whenSelectionChangedDo: [ :selection | aBlock cull: selection selectedItem ]
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> whenSelectedItemChangedDo: aBlock [
	"Set a block to value when the select item is changed"

	self
		deprecated: 'Use #whenSelectionChangedDo: instead'
		transformWith: '`@receiver whenSelectedItemChangedDo: `@argument'
						-> '`@receiver whenSelectionChangedDo: [ :selection | `@argument cull: selection selectedItem ]'.
	
	self selection whenChangedDo: [ :selection | aBlock cull: selection selectedItem ]
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> whenSelectedItemsChanged: aBlock [
	"Set a block to value when the select item is changed"
	
	self
		deprecated: 'Use #whenSelectionChangedDo: instead'
		transformWith: '`@receiver whenSelectedItemsChanged: `@argument'
						-> '`@receiver whenSelectionChangedDo: [ :selection | `@argument cull: selection selectedItems ]'.

	self selection whenChangedDo: [ :selection | aBlock cull: selection selectedItems ]
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> whenSelectionChanged: aBlock [
	
	self
		deprecated: 'Use #whenSelectionChangedDo: instead'
		transformWith: '`@receiver whenSelectionChanged: `@argument'
						-> '`@receiver whenSelectionChangedDo: `@argument'.
	^ self whenSelectionChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> whenSelectionIndexChanged: aBlock [
	
	self
		deprecated: 'Use #selection whenChangedDo: instead'
		transformWith: '`@receiver whenSelectionIndexChanged: `@argument'
						-> '`@receiver selection whenChangedDo: [ :selection | `@argument value: selection selectedIndex ]'.
	^ self  selection whenChangedDo: [ :selection | aBlock value: selection selectedIndex ]
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractListPresenter >> whenSelectionIndexesChanged: aBlock [
	"Set a block to value when the selection index has changed"
	
	self
		deprecated: 'Use #whenSelectionChangedDo: instead'
		transformWith: '`@receiver whenSelectionIndexesChanged: `@argument'
						-> '`@receiver whenSelectionChangedDo: [ :selection | `@argument value: selection selectedIndexes ]'.
	
	self whenSelectionChangedDo: [ :selection | aBlock value: selection selectedIndexes ].
]
