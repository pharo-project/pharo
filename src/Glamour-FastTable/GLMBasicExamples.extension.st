Extension { #name : #GLMBasicExamples }

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastListDoubleHeight [
	"self new fastListDoubleHeight openOn: (Smalltalk allClassesAndTraits)"

	<glmBrowser: 'Fast List With Double Height' input: 'Smalltalk allClassesAndTraits'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastList 
			display: Smalltalk allClassesAndTraits;
			rowHeight: [ :presentation | presentation class defaultRowHeight * 2 ] ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastListDoubleHeightCustomFormat [
	"self new fastListDoubleHeightCustomFormat openOn: (Smalltalk allClassesAndTraits)"

	<glmBrowser: 'Fast List With Double Height And Custom Format' input: 'Smalltalk allClassesAndTraits'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastList 
			display: Smalltalk allClassesAndTraits;
			rowHeight: [ :presentation | presentation class defaultRowHeight * 2 ];
			format: [ :aClass | 
				(Smalltalk ui theme newColumnIn: World for: { 
					aClass asStringMorph. 
					(' with ', aClass methods size printString, ' methods') asStringMorph })
						cellInset: 0; yourself ] ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastListDoubleHeightCustomFormatAndTags [
	"self new fastListDoubleHeightCustomFormatAndTags openOn: (Smalltalk allClassesAndTraits)"

	<glmBrowser: 'Fast List With Double Height And Custom Format And Tags' input: 'Smalltalk allClassesAndTraits'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastList 
			display: Smalltalk allClassesAndTraits;
			rowHeight: [ :presentation | presentation class defaultRowHeight * 2 ];
			format: [ :aClass | 
				(Smalltalk ui theme newColumnIn: World for: { 
					aClass asStringMorph. 
					(' with ', aClass methods size printString, ' methods') asStringMorph })
						cellInset: 0; 
						hResizing: #shrinkWrap;
						vResizing: #shrinkWrap;
						yourself ];
			tags: [ :aClass | aClass methods size asString, ' methods' ] ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastListDragAndDrop [
	<glmBrowser: 'Fast List drag&drop' input: '(1 to: 5) asOrderedCollection'>
	"self new fastListDragAndDrop openOn: (1 to: 5) asOrderedCollection"
	| browser |
	browser := GLMTabulator new.
	browser column: #source; column: #target.
	browser transmit to: #source; andShow: [:a | 
		a fastList 
			title: 'Source';
			display: (1 to: 10);
			allowItemDrag: [:item :list | true ];
			transformDraggedItem: [:item :list | item + 100 ]
			].
	browser transmit to: #target; andShow: [:a |  
		a fastList
			title: 'Target'; 
			display: [:collection | collection ];
			allowDropOnItem: [:draggedObject :targetItem :list | draggedObject isNumber ];
			dropOnItem: [:draggedObject :targetItem :list | 
				list entity addLast: (targetItem + draggedObject).
				list update.
				true ]
			].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastListDropDown [
	"self new fastListDropDown openOn: 5"
	<glmBrowser: 'Fast List Drop-down' input: '5'>

	|browser |

	browser := GLMTabulator new.
	browser row: #one size: 30; row: #two.
	browser transmit to: #one; andShow: [:a |
		a dropDownList
			display: [:x | 1 to: x ];
			format: [:number | Text string: number printString attribute: TextColor red]].
	browser transmit from: #one; to: #two; andShow: [:a |
		a fastList
			display: [:x | 1 to: x * 2]].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastListFunctions [
	"self new fastListFunctions openOn: ((Smalltalk allClassesAndTraits sort: [ :a :b | a asString < b asString ]) asArray)"

	"This can be done for a fastlist, a fastlist with separators, a fasttable and even a fastftree."

	<glmBrowser: 'Fast List with function' input: '(Smalltalk allClassesAndTraits sort: [ :a :b | a asString < b asString ]) asArray '>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow:
				[ :a | 
			a fastList
				title: 'Search';
				display: [ :input | input ];
				enableSearch.
			a fastList
				title: 'Filter';
				display: [ :input | input ];
				enableFilter.
			a fastList
				title: 'Filter and action';
				display: [ :input | input ];
				enableFilterWithAction: [ :dataSource :pattern :selectedRow | selectedRow browse ].
			a fastList
				title: 'Filter and named action ';
				display: [ :input | input ];
				enableFilterWithAction: [ :dataSource :pattern :selectedRow | selectedRow browse ] named: 'Browse' ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastListMultipleSelection [
	"self new fastListMultipleSelection openOn: 10"
	<glmBrowser: 'Fast List Multiple Selection' input: '10'>

	|browser |

	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [:a |
		a fastList
			beMultiple;
			display: [:x | 1 to: x ]].
	browser transmit from: #one; to: #two; andShow: [:a |
		a fastList ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastListOutlineSimple [
	"self new fastListOutlineSimple openOn: Object"

	<glmBrowser: 'Fast List Outline Simple' input: 'Object'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | 
			a fastList
				title: 'Example with an Outline List';
				display: [ :x | x allSubclasses collect: [ :each | each allSubclasses ] ];
				withSeparators ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastListOutlineWithTag [
	"self new fastListOutlineWithTag openOn: Object"

	<glmBrowser: 'Fast List Outline with Tag' input: 'Object'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow:
				[ :a | 
			a fastList
				title: 'Example with an Outline List';
				display: [ :x | x allSubclasses collect: [ :each | each allSubclasses ] ];
				withSeparators;
				tags: [ :item | item asString first asString ] ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastListSimple [
	"self new fastListSimple openOn: (Smalltalk allClassesAndTraits)"

	<glmBrowser: 'Fast List Simple' input: 'Smalltalk allClassesAndTraits'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastList display: Smalltalk allClassesAndTraits ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastTableSimple [
	<glmBrowser: 'Fast Table simple' input: '100'>
	"| f | 
	f := self new simpleTable.
	f openOn: 1000.
	(f panes first port: #selection) value: 1"
	| wrapper |	
	wrapper := GLMWrapper new.
	wrapper show: [:a | 
		a fastTable
			display: [ :x | 1 to: x ]; 
			column: [:x | 'Numbers from 1 to ', x asString] evaluated: #asString;
			column: 'Even' evaluated: [ :each | each even asString ];
			column: 'Odd' evaluated: [ :each | each odd asString ] ].
	^ wrapper
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastTableWithIcons [
	<glmBrowser: 'Fast Table with icons' input: '($A to: $z)'>
	"self new tableWithIcons openOn: ($A to: $z)"
	|browser |
	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [ :a |
		a act: [:presentation | presentation inspect] entitled: 'Inspect'. 
		a fastTable
			title: 'Table with actions';
			act: [:presentation | presentation inspect] entitled: 'Inspect'; 
			column: 'Character' evaluated: [ :each | each asString ];
			column: 'ASCII' evaluated: [ :each | each asInteger printString ];
			selectionAct: [:tree | tree selection inspect ] on: $i entitled: 'Inspect';
			icon: [:each | 
				each asInteger odd 
					ifTrue: [ GLMUIThemeExtraIcons glamorousRedCircle ] 
					ifFalse: [ GLMUIThemeExtraIcons glamorousGreenCircle ] ]].
	
	browser transmit 
		to: #two; 
		from: #one; 
		andShow: [:a | a text ].
	
	browser transmit
		from: #one port: #strongSelection;
		to: #two;
		andShow: [ :a | a text format: [ :item | 'STRONG: ', item asString ] ].
	
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastTableWithSearch [
	<glmBrowser: 'Fast Table with search' input: '100'>
	"| f | 
	f := self new fastTableWithSearch.
	f openOn: 1000.
	(f panes first port: #selection) value: 1"
	| wrapper |	
	wrapper := GLMWrapper new.
	wrapper show: [:a | 
		a fastTable
			display: [ :x | 1 to: x ]; 
			column: [:x | 'Numbers from 1 to ', x asString] evaluated: #asString;
			column: 'Even' evaluated: [ :each | each even asString ];
			column: 'Odd' evaluated: [ :each | each odd asString ];
			enableSearch;
			allowDeselection ].
	^ wrapper
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastTreeSimple [
	"self new fastTreeSimple openOn: (GLMPresentation allSubclasses sort: [:a :b | a asString < b asString])"

	<glmBrowser: 'Fast Tree Simple' input: '(GLMPresentation allSubclasses sort: [:a :b | a asString < b asString])'>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | 
			a fastTree
				title: 'Fast Tree';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ].
			a fastTree
				title: 'Fast Expanded';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				allExpanded.
			a fastTree
				title: 'Fast Tree Root Expanded';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				rootsExpanded.
			a fastTree
				title: 'Fast Tree Expanded Until 1';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				expandedUntil: 1.
				 ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastTreeStrategies [
	"self new fastTreeStrategies openOn: (GLMPresentation allSubclasses sort: [:a :b | a asString < b asString])"

	<glmBrowser: 'Fast Tree Function Strategies' input: '(GLMPresentation allSubclasses sort: [:a :b | a asString < b asString])'>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | 
			a fastTree
				title: 'Fast Tree Filter Visibles';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				enableFilter; "Could be search also"
				strategy: #default. "Optional in this case."
			a fastTree
				title: 'Fast Tree Filter Roots';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				enableFilter;
				strategy: #rootsOnly.
			a fastTree
				title: 'Fast Tree Filter All';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				enableFilter;
				strategy: #allItems.
			a fastTree
				title: 'Fast Tree Search All';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				enableSearch;
				strategy: #allItems.
				 ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastTreeWithColumnsAndEditing [
	"self new fastTreeWithColumnsAndEditing openOn: ((1 to: 5) collect: [ :index1 |
	((1 to: 10) collect: [ :index | index -> (ValueHolder new contents: 'description') ]) asDictionary ]) "

	<glmBrowser: 'Fast Tree With Editable Columns' input: '((1 to: 5) collect: [ :index1 |
	((1 to: 10) collect: [ :index | index -> (ValueHolder new contents: ''description'') ]) asDictionary ])'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastTreeTable 
			children: [ :x | x isDictionary ifTrue: [ x associations ] ifFalse: [ #() ] ];
			column: [:x | 'Numbers'] evaluated: [ :value | value isDictionary ifTrue: [ value ] ifFalse: [ value key ] ];
			column: 'Description' 
			   evaluated: [ :value | value isDictionary ifTrue: [ '' ] ifFalse: [ value value contents ] ] 
			   modified: [ :newText :value :presentation | 
					value isDictionary ifFalse: [ value value contents: newText  ]] ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastTreeWithColumnsSimple [
	"self new fastTreeWithColumnsSimple openOn: (1 to: 10)"

	<glmBrowser: 'Fast Tree With Columns' input: '(1 to: 10)'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastTreeTable 
			children: [ :x | 1 to: x-1 ];
			column: [:x | 'Numbers from 1 to ', x asString] evaluated: #asString;
			column: 'Even' evaluated: [ :each | each even asString ];
			column: 'Odd' evaluated: [ :each | each odd asString ];
			icon: [ :each | 
				each asInteger odd
					ifTrue: [ GLMUIThemeExtraIcons glamorousRedCircle ]
					ifFalse: [ GLMUIThemeExtraIcons glamorousGreenCircle ] ] ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastTreeWithIcon [
	"self new fastTreeWithIcon openOn: (1 to: 10)"

	<glmBrowser: 'Fast Tree With Icon' input: '(1 to: 10)'>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow:
				[ :a | 
			a fastTree
				title: 'Fast Tree';
				children: [ :x | 1 to: x - 1 ];
				icon: [ :each | 
					each asInteger odd
						ifTrue: [ GLMUIThemeExtraIcons glamorousRedCircle ]
						ifFalse: [ GLMUIThemeExtraIcons glamorousGreenCircle ] ] ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> fastTreeWithLazyChildren [
	"self new fastTreeWithLazyChildren openOn: (1 to: 10)"

	<glmBrowser: 'Fast Tree Lazy Children' input: '(1 to: 10)'>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | 
			a fastTree
				title: 'Fast Tree';
				hasChildren: [ :x | x > 2 ];
				children: [ :x | 1 to: x - 1 ] ].
	^ browser
]

{ #category : #'*Glamour-FastTable' }
GLMBasicExamples >> tagBlockWith: num [
	| tagBlock |
	tagBlock := [ :item | 
	item isNumber
		ifTrue:
			[ {(item even
				ifTrue: [ 'even' ]
				ifFalse: [ 'odd' ]).
			(item < num
				ifTrue: [ '<' , num asString ]
				ifFalse: [ '>=' , num asString ])} ] ].
	^ tagBlock
]
