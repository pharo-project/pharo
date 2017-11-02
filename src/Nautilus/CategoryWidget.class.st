"
CategoryWidget is an abstraction describing a widget used to manage categories

Note however that the design is ugly because it mixes UI and Model. 
All the code in this hierarchy is conceptually bogus and should be rewritten.
"
Class {
	#name : #CategoryWidget,
	#superclass : #AbstractWidget,
	#instVars : [
		'categories',
		'categoriesList',
		'categoriesSelection'
	],
	#category : 'Nautilus-Widgets'
}

{ #category : #shortcuts }
CategoryWidget class >> buildProtocolShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #fullBrowse)
		category: #NautilusProtocolShortcuts
		default: $b meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser on the selection'.
		
	(aBuilder shortcut: #restrictedBrowseClass)
		category: #NautilusProtocolShortcuts
		default: $b meta shift
		do: [ :target | target restrictedBrowseClass ]
		description: 'Open a restricted browser'.
		
	(aBuilder shortcut: #findMethod)
		category: #NautilusProtocolShortcuts
		default: $f meta
		do: [ :target | target findMethod ]
		description: 'Find a method'.
	
	(aBuilder shortcut: #addCategory)
		category: #NautilusProtocolShortcuts
		default: $n meta
		do: [ :target | target addCategory ]
		description: 'Add a new protocol'.
	
	(aBuilder shortcut: #renameCategory)
		category: #NautilusProtocolShortcuts
		default: $r meta
		do: [ :target | target enableCategorySingleSelection ifTrue: [target renameCategory ]]
		description: 'Rename the selected protocol'.
	
	(aBuilder shortcut: #removeCategories)
		category: #NautilusProtocolShortcuts
		default: $x meta
		do: [ :target | target removeCategories ]
		description: 'Remove the selected protocols'.
]

{ #category : #menus }
CategoryWidget class >> categoriesMenu: aBuilder [
	<nautilusGlobalProtocolMenu>
	| target |
	
	target := aBuilder model.
	
	(aBuilder item: #'Find Method...')
		keyText: 'f, m' if: Nautilus useOldStyleKeys not;
		keyText: 'f' if: Nautilus useOldStyleKeys;
		action: [ target findMethod ];
		order: 0;
		help: 'Search for a method by name'.
			
	(aBuilder item: #'Add protocol...')
		keyText: 'n, t' if: Nautilus useOldStyleKeys not;
		action: [ target addCategory ];
		order: 100;
		withSeparatorAfter.
	
	(aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [ target fullBrowse ];
		order: 200.
			
	(aBuilder item: #'Browse External Package')
		enabledBlock: [ target enableCategorySingleSelection and: [ target selectedCategories anySatisfy: [ :cat | cat beginsWith: '*' ] 	] ];
		action: [ (RPackage organizer packageForProtocol: target selectedCategory inClass: target class) browse ];
		order: 250;
		withSeparatorAfter.
		
	(aBuilder item: #'Categorize all uncategorized and remove empty')
		keyText: 'h, C' if: Nautilus useOldStyleKeys not;
		action: [ target categorizeAllUncategorizedMethods ];
		order: 1100.	
		
	(aBuilder item: #'Remove empty protocols')
		action: [ target removeEmptyCategories ];
		order: 1200;
		withSeparatorAfter.
		
	target selectedCategory ifNil: [ ^ target ].
	
	(aBuilder item: #'Rename...')
		keyText: 'r, t' if: Nautilus useOldStyleKeys not;
		keyText: 'r' if: Nautilus useOldStyleKeys;
		action: [ target renameCategory ];
		order: 1300;
		enabledBlock: [ target enableCategorySingleSelection ].	
		
	(aBuilder item: #'Remove...')
		keyText: 'x, t' if: Nautilus useOldStyleKeys not;
		keyText: 'x' if: Nautilus useOldStyleKeys;
		action: [ target removeCategories ];
		order: 1400;
		iconName: #removeIcon;
		withSeparatorAfter.
	
	(aBuilder item: #'File Out')
		action: [ target fileOutCategories ];
		order: 1600.
]

{ #category : #'item creation' }
CategoryWidget >> buildCategoriesList [
	^ categoriesList := self class listClass new
		basicWrapSelector: #categoryWrapper:;
		resetListSelector: #resetCategorySelection;
		keystrokeSelector: #keyPressedOnCategory:;
		autoDeselect: true;
		getListSizeSelector: #categoryListSize;
		
		dropItemSelector: #dropMethod:inARow:;
		dragEnabled: true; dropEnabled: true;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		
		getIconSelector: #categoryIconFor:; "WARNING: This needs to be set before the model"
		model: self;
		doubleClickSelector: #doubleClick:;
		getIndexSelector: #selectedCategoryIndex;
		setIndexSelector: #selectedCategoryIndex:;
		getSelectionListSelector: #categorySelectionAt:;
		setSelectionListSelector: #categorySelectionAt:put:;
		getMenuSelector: #categoriesMenu:shifted:;
		beMultipleSelection;
		
		getListElementSelector: #getCategoryItem:;
		
		changed;
		
		yourself
	

]

{ #category : #private }
CategoryWidget >> categoriesLabel [

	^ self showInstance
		ifTrue: ['Instance protocols:' asText]
		ifFalse: ['Class protocols:' asText allBold ]
]

{ #category : #protocol }
CategoryWidget >> categoriesMenu: aMenuMorph shifted: aBoolean [ 

	^ self model categoriesMenu: aMenuMorph shifted: aBoolean 
]

{ #category : #protocol }
CategoryWidget >> categoriesSelection [

	^ categoriesSelection
]

{ #category : #icon }
CategoryWidget >> categoryIconFor: aString [
	(aString beginsWith: '*')
		ifTrue: [ ^ self iconNamed: #protocolExtensionIcon ].
	(aString beginsWith: 'private')
		ifTrue: [ ^ self iconNamed: #protocolPrivateIcon ].
	(aString = 'initialization'
		or: [ aString = 'initialize-release' or: [ aString = 'finalization' ] ])
		ifTrue: [ ^ self iconNamed: #protocolProtectedIcon ].
	^ self iconNamed: #protocolNoneIcon
]

{ #category : #private }
CategoryWidget >> categoryListSize [

	^ self getCategories size
]

{ #category : #selection }
CategoryWidget >> categorySelectionAt: anIndex [

	| elt |
	elt := self getCategories at: anIndex ifAbsent: [ nil ].
	^ categoriesSelection at: elt ifAbsent: [ false ].
]

{ #category : #selection }
CategoryWidget >> categorySelectionAt: anIndex put: aBoolean [
	| elt |
	
	elt := self getCategories at: anIndex ifAbsent: [ nil ].
	categoriesSelection at: elt put: aBoolean.
	self model categorySelectionChanged.
	^ aBoolean
]

{ #category : #selection }
CategoryWidget >> categorySelectionNamed: aString put: aBoolean [
	
	categoriesSelection at: aString put: aBoolean.
	self model categorySelectionChanged.
	^ aBoolean
]

{ #category : #private }
CategoryWidget >> categoryWrapper: anItem [
	| package class result |

	package := self model selectedPackage.
	class := self model selectedClass.
	result := anItem asMorph.
	anItem first == $*
		ifTrue: [ | item |
			item := anItem allButFirst asLowercase.
			((item = package name asLowercase) or: 
				[ (item beginsWith: package name asLowercase) and: [ (item at: (package name size +1)) =$-]])
				ifFalse: [ result := anItem asMorph color: self model extensionColor; yourself ]]
		ifFalse: [
			((package extendedClasses includes: class) or: [ (package includesClass: class) not ])
				ifTrue: [ result := anItem asMorph color: self model extensionColor; yourself ]].
	self model showInstance
		ifFalse: [ result emphasis: 1 ].
	^ result
]

{ #category : #private }
CategoryWidget >> deselectProtocol: aString [

	categoriesSelection at: aString put: false
]

{ #category : #protocol }
CategoryWidget >> doubleClick: ann [
	ann event shiftPressed ifTrue: [ self openFloatingEditorToRenameAtIndex: ann selectedRowIndex ]
]

{ #category : #'drag and drop' }
CategoryWidget >> dragPassengersFor: item inMorph: dragSource [ 

	| transferType object |
	
	(dragSource isKindOf: self class listClass) ifFalse: [^ nil ].
	transferType := self dragTransferTypeForMorph: dragSource.
	object := item originalObject.
	transferType == #getCategoryItem: ifTrue: [ ^ self selectedCategories ifEmpty: [ { object } ] ].
	^ nil
]

{ #category : #'drag and drop' }
CategoryWidget >> dropMethod: aCollectionOfMethods inARow: aRow [

	self model dropMethod: aCollectionOfMethods inARow: aRow
]

{ #category : #protocol }
CategoryWidget >> getCategories [

	^ categories ifNil: [ categories := self loadCategories ].
]

{ #category : #private }
CategoryWidget >> getCategoryItem: anIndex [

	^ self getCategories at: anIndex
]

{ #category : #private }
CategoryWidget >> hasFocus [
	
	^ categoriesList hasKeyboardFocus
]

{ #category : #initialization }
CategoryWidget >> initialize [

	super initialize.
	categoriesSelection := Dictionary new.
]

{ #category : #protocol }
CategoryWidget >> keyPressedOnCategory: anEvent [
	| aCharacter |

	aCharacter := anEvent keyCharacter.

	(aCharacter == self model class nextFocusKey) ifTrue: [ ^ self model giveFocusTo: self model methodWidget ].
	(aCharacter == self model class previousFocusKey) ifTrue: [ ^ self model giveFocusTo: self model classWidget ].
	
]

{ #category : #protocol }
CategoryWidget >> label: aString [

	"categoriesGroup label: aString"
]

{ #category : #private }
CategoryWidget >> loadCategories [
	| class |
	class := self model selectedClass.
	class ifNil: [ ^ #() ].
	^ self loadPackagesCategoriesFor: class
]

{ #category : #private }
CategoryWidget >> loadGroupsCategoriesFor: class [
	| env |
	
	env := model browsedEnvironment.
	^ env isSystem
		ifTrue: [ self loadGroupsCategoriesInSytemEnvironmentFor: class ]
		ifFalse: [ self loadGroupsCategoriesInARestrictedEnvironment: env for: class ]
]

{ #category : #private }
CategoryWidget >> loadGroupsCategoriesInARestrictedEnvironment: env for: class [
	| group |
	
	group := self model selectedGroup.
	^ self sortCategories: ((group protocolsFor: class ) intersection: ( env protocolsFor: class )) forClass: class
]

{ #category : #private }
CategoryWidget >> loadGroupsCategoriesInSytemEnvironmentFor: class [
	| group |

	group := self model selectedGroup.
	^ self sortCategories: (group protocolsFor: class ) forClass: class
]

{ #category : #private }
CategoryWidget >> loadPackagesCategoriesFor: class [
	| env |
	
	env :=  self model browsedEnvironment.
	^env isSystem
		ifTrue: [ self loadPackagesCategoriesInSystemEnvironmentFor: class ]
		ifFalse: [ self loadPackagesCategoriesInARestrictedEnvironment: env for: class  ]
]

{ #category : #private }
CategoryWidget >> loadPackagesCategoriesInARestrictedEnvironment: env for: class [
	^ self 
		sortCategories: 
			((model browsedEnvironment protocolsFor: class) copyWithFirst:  AllProtocol defaultName)
		forClass: class
]

{ #category : #private }
CategoryWidget >> loadPackagesCategoriesInSystemEnvironmentFor: class [
	
	^ self 
		sortCategories: class organization allProtocolsNames 
		forClass: class
]

{ #category : #accessing }
CategoryWidget >> model: aModel [
	
	super model: aModel.
	self selectedCategory
		ifNotNil: [:cat | categoriesSelection at: cat put: true ]
]

{ #category : #updating }
CategoryWidget >> okToChange [

	^ self model okToChange
]

{ #category : #protocol }
CategoryWidget >> openFloatingEditorToRenameAtIndex: anIndex [
	| selectedRowMorph ed cat catMorph vScroll font cls |
	cls := self model selectedClass.
	cat := self getCategoryItem: anIndex.
	(cls organization protocolNamed: cat)
		ifNil: [ ^ self ]
		ifNotNil: [ :p | 
			p canBeRenamed
				ifFalse: [ ^ self ] ].
	vScroll := self vScrollValue.
	selectedRowMorph := categoriesList visibleRowMorphAtIndex: anIndex.
	catMorph := selectedRowMorph lastSubmorph lastSubmorph.
	font := catMorph font.
	ed := RubFloatingEditorBuilder new
		customizeEditorWith:
				[ :editor | 
			| right |
			editor font: font.
			editor scrollbarsShowNever.
			right := categoriesList isVerticalScrollBarVisible
				ifTrue: [ categoriesList verticalScrollBar left ]
				ifFalse: [ selectedRowMorph right ].
			editor
				bounds:
					(catMorph topLeft corner: right @ (selectedRowMorph bottom)) ];
		withEditedContentsDo:
				[ :editedContents | 
			| newName |
			newName := editedContents asString trimBoth.
			(newName = cat asString or: [ newName isEmpty])
				ifFalse: [ self renameCategory: cat toBe: newName.
					self vScrollValue: vScroll ] ].
	ed openEditorWithContents: cat asString thenDo: [ :editor | editor selectAll ]
]

{ #category : #private }
CategoryWidget >> renameCategory: oldCatString toBe: newCatString [
	| cls |
	cls := self model selectedClass.
	cls organization renameCategory: oldCatString toBe: newCatString.
	self deselectProtocol: oldCatString asSymbol.
	self model selectedCategory: newCatString asSymbol.
	self selectProtocol: newCatString asSymbol.
	self model updateCategoryAndMethodList
]

{ #category : #protocol }
CategoryWidget >> resetCategoryList [

	categories := nil
]

{ #category : #protocol }
CategoryWidget >> resetCategorySelection [

	categoriesSelection removeAll
]

{ #category : #protocol }
CategoryWidget >> searchedElement: index [

	categoriesList searchedElement: index
]

{ #category : #private }
CategoryWidget >> selectProtocol: aString [

	categoriesSelection at: aString put: true
]

{ #category : #protocol }
CategoryWidget >> selectedCategories [

	| associations |
	associations := self categoriesSelection associations select: [:assoc | assoc value ].
	associations := associations collect: [:assoc | assoc key ].
	^ associations select: [:each | each notNil ]
]

{ #category : #protocol }
CategoryWidget >> selectedCategory [

	^ self model selectedCategory
]

{ #category : #selection }
CategoryWidget >> selectedCategoryIndex [

	^ self getCategories indexOf: self selectedCategory ifAbsent: [ 0 ]
]

{ #category : #selection }
CategoryWidget >> selectedCategoryIndex: anInteger [

	| anObject |
	anObject := self getCategories at: anInteger ifAbsent: [ nil ].
	self model selectedCategory: anObject.
	
	self changed: #selectedCategoryIndex.
	self model categorySelectionChanged.
	self model changed: #currentHistoryIndex.
]

{ #category : #protocol }
CategoryWidget >> showInstance [

	^ self model showInstance
]

{ #category : #private }
CategoryWidget >> sortCategories: protocols forClass: class [
	^ self 
		sortCategories: protocols
		forClass: class
		firstInPackage: self model selectedPackage
]

{ #category : #private }
CategoryWidget >> sortCategories: allProtocols forClass: class firstInPackage: currentPackage [
	"List the protocols in this order: 
		1. the -- all -- category
		2. the extension protocols of the given package
		3. the extension protocols
		4. the normal protocols"
		
	|  sortBlock currentPackageProtocols otherProtocols |

	currentPackageProtocols := allProtocols reject: [ :protocolName | protocolName beginsWith: '*' ].
	
	"Filter out the existing protocols so that we can sort them later separately"
	otherProtocols := allProtocols difference: currentPackageProtocols.
		
	sortBlock := [ :a :b | (a compare: b caseSensitive: false) = 1 ].
	
	^ (currentPackageProtocols sort: sortBlock), (otherProtocols sort: sortBlock)
]

{ #category : #protocol }
CategoryWidget >> takeKeyboardFocus [

	categoriesList takeKeyboardFocus
]

{ #category : #protocol }
CategoryWidget >> updateCategoryList [
	self changed: #getCategoryItem:
]

{ #category : #protocol }
CategoryWidget >> vScrollValue [

	^ categoriesList scrollValue y
]

{ #category : #protocol }
CategoryWidget >> vScrollValue: aNumber [

	^ categoriesList vScrollValue: aNumber
]

{ #category : #'drag and drop' }
CategoryWidget >> wantsDroppedMorph: transferMorph event: anEvent inMorph: destinationLM [ 
	"We are only interested in TransferMorphs as wrappers for             
	informations. If their content is really interesting for us, will             
	determined later in >>acceptDroppingMorph:event:."

	| srcType dstType |

	"only want drops on lists (not, for example, on pluggable texts)"
	(destinationLM isKindOf: self class listClass) ifFalse: [ ^ false ].

	srcType := transferMorph dragTransferType.
	dstType := self dragTransferTypeForMorph: destinationLM.

	^ srcType == #getMethodItem: and: [ dstType == #getCategoryItem:]
]
