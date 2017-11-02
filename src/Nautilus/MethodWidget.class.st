"
MethodWidget is the basic implementation of a wiget managing methods.
Note however that the design is ugly because it mixes UI and Model. 
All the code in this hierarchy is conceptually bogus and should be rewritten.
"
Class {
	#name : #MethodWidget,
	#superclass : #AbstractWidget,
	#instVars : [
		'methodsList',
		'methodsSelection',
		'methods'
	],
	#classVars : [
		'MethodsIconsCache'
	],
	#category : 'Nautilus-Widgets'
}

{ #category : #shortcuts }
MethodWidget class >> buildMethodShortcutsOn: aBuilder [
	<keymap>	

	(aBuilder shortcut: #fullBrowse)
		category: #NautilusMethodShortcuts
		default: $b meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser on the selection'.
		
	(aBuilder shortcut: #restrictedBrowseClass)
		category: #NautilusMethodShortcuts
		default: $b meta shift
		do: [ :target | target restrictedBrowseClass ]
		description: 'Open a restricted browser'.
		
	(aBuilder shortcut: #categorizeMethod)
		category: #NautilusMethodShortcuts
		default: $c meta shift
		do: [ :target | target categorizeMethod ]
		description: 'Recategorize the selected methods'.
	
	(aBuilder shortcut: #findMethod)
		category: #NautilusMethodShortcuts
		default: $f meta
		do: [ :target | target findMethod ]
		description: 'Find a method'.
	
	(aBuilder shortcut: #methodHierarchy)
		category: #NautilusMethodShortcuts
		default: $i meta
		do: [ :target | target enableMethodSingleSelection ifTrue: [ target methodHierarchy ]]
		description: 'Spawn selected method hierarchy'.
		
	(aBuilder shortcut: #generateTestMethodsAndFocus)
		category: #NautilusMethodShortcuts
		default: $j meta
		do: [ :target | target generateTestMethodsAndFocus: true ]
		description: 'Generate test methods for the selected methods and jump to them'.
		
	(aBuilder shortcut: #generateTestMethods)
		category: #NautilusMethodShortcuts
		default: $j meta shift
		do: [ :target | target generateTestMethodsAndFocus: false ]
		description: 'Generate test methods for the selected methods'.
		
	(aBuilder shortcut: #browseMessages)
		category: #NautilusMethodShortcuts
		default: $m meta
		do: [ :target | target enableMethodSingleSelection ifTrue: [ target browseMessages ]]
		description: 'Browse implementors'.
	
	(aBuilder shortcut: #browseSendersOfMessages)
		category: #NautilusMethodShortcuts
		default: $n meta
		do: [ :target | target enableMethodSingleSelection ifTrue: [ target browseSendersOfMessages ]]
		description: 'Browse senders'.
	
	(aBuilder shortcut: #runTestFor)
		category: #NautilusMethodShortcuts
		default: $t meta
		do: [ :target | target runTestForMethods: target selectedMethods notifying: true]
		description: 'Run the selected test methods'.
		
	(aBuilder shortcut: #browseVersions)
		category: #NautilusMethodShortcuts
		default: $v meta
		do: [ :target | target enableMethodSingleSelection ifTrue: [ target browseVersions ]]
		description: 'Browse version'.		
	
	(aBuilder shortcut: #removeMethods)
		category: #NautilusMethodShortcuts
		default: $x meta
		do: [ :target | target removeMethods ]
		description: 'Remove the selected methods'.
		
	(aBuilder shortcut: #toggleBytecodes)
		category: #NautilusMethodShortcuts
		default: $b meta
		do: [ :target | target toggleBytecodes ]
		description: 'ShowBytecodes'.
]

{ #category : #menu }
MethodWidget class >> elementsMenu: aBuilder [
	<nautilusGlobalMethodMenu>
	| selectedMethods tmp target |
	target := aBuilder model.
	selectedMethods := target selectedMethods.
	
	(aBuilder item: #'ToggleBreakPoint')
		action: [ target toggleBreakpoint ];
		order: 150;
		enabledBlock: [ target selectedMethod notNil ];
		label: ((target selectedMethod isNil or: [ target selectedMethod hasBreakpoint not ])
				ifTrue: [ 'Add breakpoint']
				ifFalse: [ 'Remove breakpoint'] ).
	
	(aBuilder item: #'ToggleBreakPointOnce')
		action: [ target toggleBreakpointOnce ];
		order: 151;
		enabledBlock: [ target selectedMethod notNil and: [ target selectedMethod hasBreakpoint not ] ];
		label: 'Add break once'.
		
	(aBuilder item: #'ToggleBreakPointCondition')
		action: [ target toggleBreakpointCondition ];
		order: 152;
		enabledBlock: [ target selectedMethod notNil and: [ target selectedMethod hasBreakpoint not ] ];
		label: 'Add break condition...';
		withSeparatorAfter.	
		

	(aBuilder item: #'Find Method...')
		keyText: 'f, m' if: Nautilus useOldStyleKeys not;
		keyText: 'f' if: Nautilus useOldStyleKeys;
		action: [ target findMethod ];
		order: 199;
		help: 'Search for a method by name'.
		
	tmp := ((aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [ target fullBrowse ];
		order: 200;
		yourself).
		
	target selectedMethod ifNil: [
		tmp withSeparatorAfter.
		 ^ target ].
	
	(aBuilder item: #'Generate test and jump')
		keyText: 'h, j' if: Nautilus useOldStyleKeys not;
		keyText: 'j' if: Nautilus useOldStyleKeys;
		action: [ target generateTestMethodsAndFocus: true ];
		order: 300.
		
	(aBuilder item: #'Generate test')
		keyText: 'h, J' if: Nautilus useOldStyleKeys not;
		keyText: 'J' if: Nautilus useOldStyleKeys;
		action: [ target generateTestMethodsAndFocus: false ];
		order: 400;
		withSeparatorAfter.
		
	(target selectedMethods allSatisfy: [:meth | meth isTestMethod ])
		ifTrue: [
			(aBuilder item: #'Run tests')
				keyText: 'j, m' if: Nautilus useOldStyleKeys not;
				keyText: 't' if: Nautilus useOldStyleKeys;
				action: [ target runTestForMethods: selectedMethods notifying: true ];
				order: 1000. ].
	
	target selectedMethod asTestMethod
		ifNotNil: [
			(aBuilder item: #'Debug test')
				keyText: 'd' if: Nautilus useOldStyleKeys not;
				action: [ target debugTest ];
				order: 1001 ].		
			
	(aBuilder item: #'Senders of...')
		keyText: 'b, n' if: Nautilus useOldStyleKeys not;
		keyText: 'n' if: Nautilus useOldStyleKeys;
		action: [ target browseSendersOfMessages ];
		order: 1100;
		enabledBlock: [ target enableMethodSingleSelection ].
		
	(aBuilder item: #'Implementors of...')
		keyText: 'b, m' if: Nautilus useOldStyleKeys not;
		keyText: 'm' if: Nautilus useOldStyleKeys;
		action: [ target browseMessages ];
		order: 1200;
		enabledBlock: [ target enableMethodSingleSelection ].
		
	(aBuilder item: #'Inheritance')
		keyText: 'b, i' if: Nautilus useOldStyleKeys not;
		keyText: 'i' if: Nautilus useOldStyleKeys;
		action: [ target methodHierarchy ];
		order: 1300;
		enabledBlock: [ target enableMethodSingleSelection ].
		
	(aBuilder item: #'Versions')
		keyText: 'm, v' if: Nautilus useOldStyleKeys not;
		keyText: 'v' if: Nautilus useOldStyleKeys;
		action: [ target browseVersions ];
		order: 1400;
		enabledBlock: [ target enableMethodSingleSelection ];
		withSeparatorAfter.
	
	(aBuilder item: #'Categorize method')
		keyText: 'm, m' if: Nautilus useOldStyleKeys not;
		action: [ target categorizeMethod ];
		order: 1500.
		
	(aBuilder item: #'Move to package...')
		action: [ target moveMethodToPackage ];
		order: 1600.
		
	(aBuilder item: #'Remove...')
		keyText: 'x, m' if: Nautilus useOldStyleKeys not;
		keyText: 'x' if: Nautilus useOldStyleKeys;
		action: [ target removeMethods ];
		order: 1700;
		iconName: #removeIcon;
		withSeparatorAfter.
		
	(aBuilder item: #'Compare two methods')		 
		action: [ target compareTwoMethods ];
		order: 1800;
		enabledBlock: [ target enableMethodDualSelection ];
		withSeparatorAfter.	
		
	(aBuilder item: #'Inspect Method')
		action: [ target inspectMethod ];
		order: 1950.
		
	(aBuilder item: #'Toggle Bytecodes')
		keyText: 'b, b' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [ target toggleBytecodes ];
		order: 1970.
			
	(aBuilder item: #'File Out')
		action: [ target fileOutMethods ];
		order: 2000
]

{ #category : #icon }
MethodWidget class >> methodsIconsCache [

	^ MethodsIconsCache ifNil: [ MethodsIconsCache := WeakIdentityKeyDictionary new ]
]

{ #category : #protocol }
MethodWidget class >> removeAllFromMethodsIconsCache: aMethod [
	self methodsIconsCache
		keysDo: [ :method | 
			" for overrides "
			method selector = aMethod selector
				ifTrue: [ MethodsIconsCache removeKey: method ifAbsent: [  ] ] ]
]

{ #category : #icon }
MethodWidget class >> resetMethodIconCache [

	MethodsIconsCache := WeakIdentityKeyDictionary new 
]

{ #category : #'item creation' }
MethodWidget >> buildMethodsList [
	methodsList := self class listClass new
		basicWrapSelector: #methodWrapper:;
		keystrokeSelector: #keyPressedOnElement:;
		getIconSelector: #methodIconFor:;
		resetListSelector: #resetMethodSelection;
		getListSizeSelector: #methodListSize;
		doubleClickSelector: #doubleClick:;
		autoDeselect: true;
		dragEnabled: true;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		basicGetListElementSelector: #getMethodItem:;
		model: self;
		getIndexSelector: #selectedMethodIndex;
		setIndexSelector: #selectedMethodIndex:;
		getSelectionListSelector: #methodSelectionAt:;
		setSelectionListSelector: #methodSelectionAt:put:;
		getMenuSelector: #elementsMenu:shifted:;
		beMultipleSelection.
	^ methodsList
]

{ #category : #protocol }
MethodWidget >> doubleClick: ann [
	ann event shiftPressed ifTrue: [ ^ self openFloatingEditorToRenameAtIndex: ann selectedRowIndex  ].
	self model fullBrowse
]

{ #category : #'drag and drop' }
MethodWidget >> dragPassengersFor: item inMorph: dragSource [ 

	| transferType object |
	(dragSource isKindOf: self class listClass) ifFalse: [^ nil ].
	transferType := self dragTransferTypeForMorph: dragSource.
	transferType == #getMethodItem: ifFalse: [ ^ nil ].
	object := item originalObject.

	^ self selectedMethods
		ifEmpty: [
			 (self getMethods includes: object)
				ifTrue: [ { object } ]]
]

{ #category : #protocol }
MethodWidget >> elementsMenu: aMenuMorph shifted: aBoolean [ 
	
	^ self model elementsMenu: aMenuMorph shifted: aBoolean 
]

{ #category : #private }
MethodWidget >> getMethodItem: anIndex [

	^ self getMethods at: anIndex
]

{ #category : #protocol }
MethodWidget >> getMethods [

	^ methods ifNil: [ methods := self loadMethods ].
]

{ #category : #private }
MethodWidget >> handleGroupWrappingFor: aSelector and: aStringMorph in: aPackage [
	aPackage name = self model model class lastModifiedMethodsGroupName 
		ifFalse: [ ^ aStringMorph ]
		ifTrue: [ 
			aPackage node item methods
				detect: [ :method | method selector = aSelector ]
				ifFound: [ ^ aSelector asMorph ]
				ifNone: [ ^ aStringMorph ] ]
]

{ #category : #private }
MethodWidget >> hasFocus [

	^ methodsList hasKeyboardFocus
]

{ #category : #initialization }
MethodWidget >> initialize [

	super initialize.
	MethodsIconsCache ifNil: [ MethodsIconsCache := WeakIdentityKeyDictionary new ].
	methodsSelection := OrderedCollection new.
]

{ #category : #protocol }
MethodWidget >> invalidateCacheEntryFor: aMethod [
	self removeAllFromMethodsIconsCache: aMethod.
	self updateMethodList
]

{ #category : #protocol }
MethodWidget >> keyPressedOnElement: anEvent [
	| aCharacter |
	
	aCharacter := anEvent keyCharacter.

	aCharacter == self model class nextFocusKey ifTrue: [ ^ self model giveFocusToSourceCodeArea ].	
	aCharacter == self model class previousFocusKey ifTrue: [ ^ self model giveFocusTo: self model categoryWidget ].

]

{ #category : #protocol }
MethodWidget >> label: aString [

	"methodsGroup label: aString"
]

{ #category : #private }
MethodWidget >> loadMethods [
	^ self model selectedCategories
		ifEmpty: [ self model methodsForCategory: self model allLabel ]
		ifNotEmpty: [ :cat | self model methodsForCategories: cat ]
]

{ #category : #icon }
MethodWidget >> methodIconFor: aMethod [

	| actions button action |
	MethodsIconsCache
		at: aMethod
		ifPresent: [:methodIcon | 
			methodIcon isArray
				ifTrue: [ ^ self rebuildUpAndDownArrowIconFrom: methodIcon].
			methodIcon class == IconicButtonStateHolder
				ifTrue: [
					"convert the IconicButtonStateHolder previously cached by
					this method with #actionStateToCache before returning"
					^ methodIcon asIcon ].
			^ methodIcon. ].

	actions := AbstractMethodIconAction allSubclasses collect: [:class | class for: aMethod in: NautilusUI ].
	actions sort: [:a :b | a actionOrder < b actionOrder ].
	action := actions 
		detect: [:each | each isActionHandled ]. 
	
	"actionIcon need to be invoked before actionStateToCache"
	button := action actionIcon.
	MethodsIconsCache at: aMethod put: action actionStateToCache.
	^ button
]

{ #category : #private }
MethodWidget >> methodListSize [
	
	^ self getMethods size
]

{ #category : #selection }
MethodWidget >> methodSelectionAt: anIndex [
	| elt |
	elt := self getMethods at: anIndex ifAbsent: [ ^ false ].
	^ methodsSelection
		detect: [ :meth | meth methodClass = elt methodClass and: [ meth selector = elt selector ] ]
		ifFound: [ :meth | meth ]
		ifNone: [ false ]
]

{ #category : #selection }
MethodWidget >> methodSelectionAt: anIndex put: aBoolean [
	| method |
	self flag: #clean.	"This method as classAt:put: and categoryAt:put: need to be clean latter. A class should not expose his implementation. Instead we should have selectMethod: unselectMethod:. Or better, this logic should be in AbstractWidget since all the subclasses remember the selected objects."
	method := self getMethods at: anIndex ifAbsent: [ ^ self ].
	aBoolean
		ifTrue: [ self selectMethod: method ]
		ifFalse: [ self removeSelectionOf: method ]
]

{ #category : #private }
MethodWidget >> methodWrapper: anItem [
	| package selector result emphasis |
	selector := anItem selector.
	package := self model selectedPackage.
	result := selector asMorph.
	emphasis := 0.
	anItem protocol asString first = $*
		ifTrue: [ 
			| item |
			item := anItem protocol allButFirst asLowercase.
			(item = package name asLowercase
				or: [ (item beginsWith: package name asLowercase) and: [ (item at: package name size + 1) = $- ] ])
				ifFalse: [ 
					result := selector asMorph
						color: self model extensionColor;
						yourself ] ]
		ifFalse: [ 
			((package extendedClasses includes: anItem methodClass) or: [ (package includesClass: anItem methodClass) not ])
				ifTrue: [ 
					result := selector asMorph
						color: self model extensionColor;
						yourself ] ].
	result := self handleGroupWrappingFor: selector and: result in: package.
	anItem isFromTrait
		ifTrue: [ 
			| trait |
			trait := anItem origin.
			selector := selector , ' (' , trait name , ')'.
			result := selector asMorph.
			emphasis := emphasis + 2 ].
	self model showInstance
		ifFalse: [ emphasis := emphasis + 1 ].
	anItem isDeprecated ifTrue: [ emphasis := emphasis + 2r10000 ].	
	^ result emphasis: emphasis
]

{ #category : #protocol }
MethodWidget >> methodsIconsCache [

	^ self class methodsIconsCache
]

{ #category : #private }
MethodWidget >> methodsLabel [

	^ self showInstance
		ifTrue: ['Instance methods' asText ]
		ifFalse: ['Class methods' asText allBold ]
]

{ #category : #private }
MethodWidget >> methodsList [
	^ methodsList
]

{ #category : #protocol }
MethodWidget >> methodsSelection [

	^ methodsSelection
]

{ #category : #protocol }
MethodWidget >> okToChange [

	^ self model okToChange. 
]

{ #category : #private }
MethodWidget >> openFloatingEditorToRenameAtIndex: anIndex [
	| selectedRowMorph ed itemRowMorph vScroll font cls method selector |
	method := self getMethods at: anIndex.
	selector := method selector.
	cls := method methodClass.
	vScroll := self vScrollValue.
	selectedRowMorph := methodsList visibleRowMorphAtIndex: anIndex.
	itemRowMorph := selectedRowMorph lastSubmorph lastSubmorph.
	font := itemRowMorph font.
	ed := RubFloatingEditorBuilder new
		customizeEditorWith:
				[ :editor | 
			| right |
			editor font: font.
			editor scrollbarsShowNever.
			right := methodsList isVerticalScrollBarVisible
				ifTrue: [ methodsList verticalScrollBar left ]
				ifFalse: [ selectedRowMorph right ].
			editor
				bounds:
					(itemRowMorph topLeft 
						corner: right @ selectedRowMorph bottom ) ];
		withEditedContentsDo:
				[ :editedContents | 
			| newName |
			newName := editedContents asString trimBoth.
			(cls selectors includes: newName asSymbol)
				ifTrue:
					[ newName asSymbol ~= selector
						ifTrue: [ self inform: newName , ' already exists in ' , cls name ] ]
				ifFalse:
					[ | oldArguments newMethodName |
					oldArguments := method ast arguments collect: #name.
					newMethodName := RBMethodName selector: newName asSymbol arguments: oldArguments.
					newMethodName isValid
						ifTrue:
							[ | refactor |
							refactor := self model refactor promptOnRefactoring: false.
							refactor renameMethodFor: method withNewMethodName: newMethodName ]
						ifFalse: [ self inform: 'Invalid method name' ].
					self vScrollValue: vScroll ] ].
	ed openEditorWithContents: selector asString thenDo: [ :editor | editor selectAll  ]
]

{ #category : #icon }
MethodWidget >> rebuildUpAndDownArrowIconFrom: array [

	| container |
	
	container := Morph new.
	container 
		extent: 16@16;
		color: Color transparent;
		changeTableLayout;
		wrapCentering: #center;
		cellPositioning: #topCenter;
		listDirection: #topToBottom.
	
	array 
	collect: [ :each | each asIconTargetting: NautilusUI ] 
	thenDo: [ :each | container addMorphBack: each ].
	
	^container
]

{ #category : #protocol }
MethodWidget >> removeAllFromMethodsIconsCache: aMethod [
	^self class removeAllFromMethodsIconsCache: aMethod
]

{ #category : #selection }
MethodWidget >> removeSelectionOf: element [
	"I do not remove immediatly the element because if the method have been compile, this will not be the same object anymore. So I check the class and the selector of the method."

	methodsSelection detect: [ :meth | meth methodClass = element methodClass and: [ meth selector = element selector ] ] ifFound: [ :meth | methodsSelection remove: meth ] ifNone: [ ^ self ]
]

{ #category : #protocol }
MethodWidget >> resetMethodList [

	methods := nil
]

{ #category : #protocol }
MethodWidget >> resetMethodSelection [

	methodsSelection := OrderedCollection new
]

{ #category : #protocol }
MethodWidget >> selectMethod: aMethod [
	aMethod
		ifNotNil: [ self removeSelectionOf: aMethod.
			methodsSelection add: aMethod ]
]

{ #category : #protocol }
MethodWidget >> selectNextMethod [
	| methodToSelect selectedMethodIndex |
	self getMethods size <= 1
		ifFalse:
			[ 
			selectedMethodIndex := self selectedMethodIndex.
			methodToSelect := selectedMethodIndex < self getMethods size
				ifTrue: [ self getMethods at: selectedMethodIndex + 1 ]
				ifFalse: [ self getMethods at: selectedMethodIndex - 1 ] ].
	self selectMethod: methodToSelect.
	self model selectedMethod: methodToSelect
]

{ #category : #protocol }
MethodWidget >> selectedMethod [

	^ self model selectedMethod
]

{ #category : #protocol }
MethodWidget >> selectedMethod: aMethod [
	"I check if it's ok here to work better with the drag/drop mechanism"

	self model selectedMethod: aMethod
]

{ #category : #selection }
MethodWidget >> selectedMethodIndex [
	| methodClass selector |
	self selectedMethod ifNil: [ ^ 0 ].
	methodClass := self selectedMethod methodClass.
	selector := self selectedMethod selector.
	^ self getMethods detectIndex: [ :method | method methodClass = methodClass and: [ method selector = selector ] ] ifNone: [ 0 ]
]

{ #category : #selection }
MethodWidget >> selectedMethodIndex: anInteger [

	| aMethod |
	aMethod :=  self getMethods at: anInteger ifAbsent: [ nil ].
	self selectedMethod: aMethod.

	self changed: #selectedMethodIndex.
]

{ #category : #protocol }
MethodWidget >> selectedMethods [
	^ self methodsSelection
]

{ #category : #protocol }
MethodWidget >> setIcon: icon for: method [

	MethodsIconsCache at: method put: icon
]

{ #category : #protocol }
MethodWidget >> showInstance [

	^ self model showInstance
]

{ #category : #protocol }
MethodWidget >> takeKeyboardFocus [

	methodsList takeKeyboardFocus
]

{ #category : #protocol }
MethodWidget >> updateMethodList [

	self update: #getMethodItem:
]

{ #category : #protocol }
MethodWidget >> updateMethodList: aMethodOrNil [
	
	self update: #getMethodItem:.
	self selectMethod: aMethodOrNil.
	aMethodOrNil ifNotNil: [  
		| index |
		index := (methods collect: [:each | each selector]) indexOf: aMethodOrNil selector ifAbsent: 0.
		self selectedMethodIndex: index.]
]

{ #category : #protocol }
MethodWidget >> vScrollValue [

	^ methodsList scrollValue y
]

{ #category : #protocol }
MethodWidget >> vScrollValue: aNumber [

	^ methodsList vScrollValue: aNumber
]
