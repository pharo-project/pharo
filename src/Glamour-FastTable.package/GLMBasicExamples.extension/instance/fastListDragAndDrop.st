*Glamour-FastTable
fastListDragAndDrop
	<glmBrowser: 'Fast List drag&drop' input: '(1 to: 5) asOrderedCollection'>
	"self new listDragAndDrop openOn: (1 to: 5) asOrderedCollection"
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