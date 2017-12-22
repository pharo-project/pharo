"
ClassWidget is the basic implementation of a widget a list of classes.

Note however that the design is ugly because it mixes UI and Model. 
All the code in this hierarchy is conceptually bogus and should be rewritten.
"
Class {
	#name : #ClassWidget,
	#superclass : #AbstractWidget,
	#instVars : [
		'classesList',
		'classesSelection',
		'classesListElements',
		'cachedHierarchy',
		'showHierarchy'
	],
	#category : #'Nautilus-Widgets'
}

{ #category : #private }
ClassWidget >> abstractColorAdjust: aColor [
			
	^aColor luminance > 0.5
			ifTrue: [ aColor twiceDarker ]
			ifFalse: [ aColor twiceLighter ]
]

{ #category : #'item creation' }
ClassWidget >> buildClassesList [
	^ classesList := self class listClass new
				basicWrapSelector: #classWrapper:;
				resetListSelector: #resetClassSelection;
				keystrokeSelector: #keyPressedOnList:shifted:;
				autoDeselect: true;
				getListSizeSelector: #classListSize;

				dropItemSelector: #dropInClass:inARow:;
				dragEnabled: true; dropEnabled: true;
				hResizing: #spaceFill;
				vResizing: #spaceFill;

				getIconSelector: #classIconFor:;
				model: self;								
				doubleClickSelector: #doubleClick:;
				getIndexSelector: #selectedClassIndex;
				setIndexSelector: #selectedClassIndex:;
				getSelectionListSelector: #classSelectionAt:;
				setSelectionListSelector: #classSelectionAt:put:;
				getMenuSelector: #classesMenu:shifted:;
				beMultipleSelection;
				
				getListElementSelector: #getClassItem: ;
				
				changed;
				
				yourself					

]

{ #category : #private }
ClassWidget >> buildPackageGroupHierarchyFor: aPackageGroup [
	^ self cachedHierarchy class buildHierarchyFor: aPackageGroup in: self model browsedEnvironment
]

{ #category : #private }
ClassWidget >> buildTabbedNameOf: anElement [
			
	| tab size stringMorph |
	
	tab := '   '.
	size := self cachedHierarchy indentationFor: anElement.
	
	stringMorph := (size isZero 
		ifTrue: [ anElement name ]
		ifFalse: [ ( tab repeat: size ) , anElement name ]) asStringMorph.
		
	self model selectedPackage ifNotNil: [ :package |
		(package includesClass: anElement) ifFalse: [ 
			stringMorph color: self model extensionColor ] 
	].
	
	anElement isAbstract ifTrue: [ 	
		stringMorph 
		emphasis: 2; 
		color: (self abstractColorAdjust: stringMorph color)
	].
	
	^ stringMorph
]

{ #category : #accessing }
ClassWidget >> cachedHierarchy [
	
	^ cachedHierarchy
]

{ #category : #accessing }
ClassWidget >> cachedHierarchy: anObject [
	
	cachedHierarchy := anObject
]

{ #category : #'private list model' }
ClassWidget >> classIconFor: anItem [
	^ self model classIconFor: anItem
]

{ #category : #'private list model' }
ClassWidget >> classListSize [
	^ self getClassesList size
]

{ #category : #'private list model' }
ClassWidget >> classSelectionAt: anIndex [
	
	| elt |
	elt := self getClassesList at: anIndex ifAbsent: [ ^false ].
	^ self classesSelection at: elt ifAbsent: [ false ]
]

{ #category : #'private list model' }
ClassWidget >> classSelectionAt: anIndex put: aBoolean [
	| elt |
	aBoolean ifNil: [ ^ self ].
	elt := self getClassesList at: anIndex ifAbsent: [ ^ self ].
	self classesSelection at: elt put: aBoolean.
			
	^ aBoolean
]

{ #category : #'private list model' }
ClassWidget >> classWrapper: anElement [
	^ self sortHierarchically
		ifTrue: [ self buildTabbedNameOf: anElement ]
		ifFalse: [  self wrapFlat: anElement  ]
]

{ #category : #'private list model' }
ClassWidget >> classesMenu: aMenu shifted: aBoolean [
	self model classesMenu: aMenu shifted: aBoolean.
	^ aMenu
]

{ #category : #private }
ClassWidget >> classesSelection [
	
	^ classesSelection
]

{ #category : #private }
ClassWidget >> createClassHierarchyFor: aPackageGroup [

	aPackageGroup ifNil: [ ^ {}].
	self cachedHierarchy: (self buildPackageGroupHierarchyFor: aPackageGroup asNautilusSelection).
	^ self cachedHierarchy sortedElements
]

{ #category : #private }
ClassWidget >> createClassListFor: aPackageGroup [

	aPackageGroup ifNil: [ ^ {}].
	^ (SortAlphabeticallyClassList buildListFor: aPackageGroup asNautilusSelection in: self model browsedEnvironment) sortedElements
]

{ #category : #'private list model' }
ClassWidget >> doubleClick [
	self model fullBrowse
]

{ #category : #'private list model' }
ClassWidget >> doubleClick: ann [
	ann event shiftPressed ifTrue: [ ^ self openFloatingEditorToRenameAtIndex: ann selectedRowIndex  ].
	self model fullBrowse
]

{ #category : #'drag and drop' }
ClassWidget >> dragPassengersFor: item inMorph: dragSource [
	| transferType object |
	(dragSource isKindOf: self class listClass)
		ifFalse: [ ^ nil ].
	transferType := self dragTransferTypeForMorph: dragSource.
	object := item originalObject.
	transferType == #getClassItem:
		ifTrue: [ ^ self selectedClasses ifEmpty: [ {object} ] ].
	^ nil
]

{ #category : #'private list model' }
ClassWidget >> dropInClass: aCollection inARow: aRow [
	| receiver |
	aRow>0 ifTrue: [  
		receiver := self getClassesList at: aRow.
		receiver := 
		self model showInstance not
			ifTrue: [ receiver theMetaClass ]
			ifFalse: [ receiver theNonMetaClass ].
		self model dropInAClass: aCollection into: receiver]
]

{ #category : #'private list model' }
ClassWidget >> getClassItem: anIndex [
	^ self getClassesList at: anIndex
]

{ #category : #protocol }
ClassWidget >> getClassesList [

	^ classesListElements ifNil: [ classesListElements := self loadClassesListElements ].
]

{ #category : #protocol }
ClassWidget >> hasFocus [
	
	^ classesList hasKeyboardFocus
]

{ #category : #initialization }
ClassWidget >> initialize [

	super initialize.

	classesList := nil.
	classesListElements := nil.
	classesSelection := Dictionary new.
	cachedHierarchy := SortHierarchically new.
	showHierarchy := false.
]

{ #category : #'private list model' }
ClassWidget >> keyPressedOnList: anEvent shifted: aBoolean [
		| aCharacter |

	aCharacter := anEvent keyCharacter.

	(aCharacter == self model class nextFocusKey) ifTrue: [ ^ self model giveFocusTo: self model categoryWidget ].
	(aCharacter == self model class previousFocusKey) ifTrue: [ ^ self model giveFocusToPackage ].
]

{ #category : #private }
ClassWidget >> loadClassHierarchyListElements [
	^ self cachedHierarchy sortedElements 
]

{ #category : #private }
ClassWidget >> loadClassesListElements [
	^ self showHierarchy
		ifTrue: [ self loadClassHierarchyListElements ]
		ifFalse: [ self loadPackageClassListElements ]
]

{ #category : #private }
ClassWidget >> loadPackageClassListElements [
	^ self sortHierarchically
		ifTrue: [ self createClassHierarchyFor: self model selectedPackageGroup ]
		ifFalse: [ self createClassListFor:self model selectedPackageGroup ]
]

{ #category : #protocol }
ClassWidget >> okToChange [
	^ model okToChange
]

{ #category : #protocol }
ClassWidget >> openFloatingEditorToRenameAtIndex: anIndex [
	| selectedRowMorph ed clsMorph vScroll font cls |
	cls := self model selectedClass theNonMetaClass.
	vScroll := self vScrollValue.
	selectedRowMorph := classesList visibleRowMorphAtIndex: anIndex.
	clsMorph := selectedRowMorph lastSubmorph lastSubmorph.
	font := clsMorph font.
	ed := RubFloatingEditorBuilder new
		customizeEditorWith:
				[ :editor | 
			| right |
			editor font: font.
			editor scrollbarsShowNever.
			right := classesList isVerticalScrollBarVisible
				ifTrue: [ classesList verticalScrollBar left ]
				ifFalse: [ selectedRowMorph right ].
			editor
				bounds:
					(clsMorph topLeft corner: right @ selectedRowMorph bottom) ];
		withEditedContentsDo:
				[ :editedContents | 
			| newName |
			newName := editedContents asString trimBoth.
			(newName = cls name asString or: [ newName isEmpty])
				ifFalse: [self renameClass: cls toBe: newName.
					self vScrollValue: vScroll ] ].
	ed openEditorWithContents: cls name asString thenDo: [ :editor | editor selectAll  ]
]

{ #category : #protocol }
ClassWidget >> renameClass: aClass toBe: aString [
	| newName oldName obs |
	aString isEmptyOrNil ifTrue: [ ^ self ].	"Cancel returns"
	newName := aString asSymbol.
	newName = aClass name asSymbol ifTrue: [ ^ self ].
	oldName := aClass name asSymbol.
	(aClass environment includesKey: newName)
		ifTrue: [ ^ self inform: newName , ' already exists' ].
	aClass rename: newName.

	obs := self model systemNavigation allReferencesTo: (aClass environment associationAt: newName).
	obs isEmpty
		ifFalse: [ self model systemNavigation browseMessageList: obs name: 'Obsolete References to ' , oldName autoSelect: oldName ].
	^ newName
]

{ #category : #protocol }
ClassWidget >> resetClassList [
	classesListElements := nil.
]

{ #category : #protocol }
ClassWidget >> resetClassSelection [
	self classesSelection removeAll
]

{ #category : #protocol }
ClassWidget >> selectClass: aClass [
	self classesSelection at: aClass theNonMetaClass put: true.

]

{ #category : #protocol }
ClassWidget >> selectedClass [
	^ self selectedClasses ifEmpty:[ nil]  ifNotEmpty:[: col | col first]
]

{ #category : #'private list model' }
ClassWidget >> selectedClassIndex [
	| item |
	item := self selectedClass.
	self model showInstance
		ifFalse: [ item ifNotNil: [ item := item theNonMetaClass ] ].
	^ self getClassesList indexOf: item
]

{ #category : #'private list model' }
ClassWidget >> selectedClassIndex: anInteger [
	| selection |
	selection := self getClassesList at: anInteger ifAbsent: [nil].
	
	self model showInstance ifFalse: [selection ifNotNil: [selection := selection theMetaClass]].
	self model selectedClassWithoutChangingSelection: selection.
	
	self model changed: #currentHistoryIndex.
]

{ #category : #protocol }
ClassWidget >> selectedClasses [

	| associations |
	associations := self classesSelection associations select: [:assoc | assoc value ].
	associations := associations collect: [:assoc | assoc key ].
	^ associations select: [:each | each notNil ]
]

{ #category : #private }
ClassWidget >> setCachedHierarchyClass: aClass [
    
	self cachedHierarchy: (SortHierarchically buildHierarchyForClass: aClass) 
]

{ #category : #protocol }
ClassWidget >> showHierarchy [
	
	^ showHierarchy
]

{ #category : #protocol }
ClassWidget >> showHierarchy: aBoolean [

	(showHierarchy := aBoolean)
		ifTrue: [ self selectedClass ifNotNil: [ :class | self setCachedHierarchyClass: class ] ]
]

{ #category : #protocol }
ClassWidget >> showHierarchy: aBoolean forClass: class [

	(showHierarchy := aBoolean)
		ifTrue: [ class ifNotNil: [ self setCachedHierarchyClass: class ] ]
]

{ #category : #accessing }
ClassWidget >> sortHierarchically [
	^ self model sortHierarchically
]

{ #category : #protocol }
ClassWidget >> takeKeyboardFocus [

	classesList takeKeyboardFocus
]

{ #category : #protocol }
ClassWidget >> vScrollValue [

	^ classesList scrollValue y
]

{ #category : #protocol }
ClassWidget >> vScrollValue: aNumber [

	^ classesList vScrollValue: aNumber
]

{ #category : #'drag and drop' }
ClassWidget >> wantsDroppedMorph: transferMorph event: anEvent inMorph: destinationLM [
	| srcType dstType |
	"only want drops on lists (not, for example, on pluggable texts)"
	(destinationLM isKindOf: self class listClass)
		ifFalse: [ ^ false ].
	srcType := transferMorph dragTransferType.
	dstType := self dragTransferTypeForMorph: destinationLM.
	srcType == #getMethodItem:
		ifTrue: [ ^ dstType == #getClassItem: ].
	srcType == #getCategoryItem:
		ifTrue: [ ^ dstType == #getClassItem: ].
	^ false
	

]

{ #category : #private }
ClassWidget >> wrapFlat: anElement [
	| result |
	result := anElement name.
	^ self model selectedPackage
		ifNil: [ result ]
		ifNotNil: [ :package | 
			(package includesClass: anElement)
				ifTrue: [ result asStringMorph ]
				ifFalse: [ 
					result asStringMorph
						color: self model extensionColor;
						yourself ] ]
]
