Extension { #name : #MenuMorph }

{ #category : #'*GT-SpotterExtensions-Core' }
MenuMorph >> allItems [
	| all |
	all := OrderedCollection new.
	self items do: [ :each |
		all add: each.
		each hasSubMenu 
			ifTrue: [ all addAll: each subMenu allItems ] ].
	^ all
]

{ #category : #'*GT-SpotterExtensions-Core' }
MenuMorph class >> gtExampleMenuMorph [
	<gtExample>
	| menu |
	menu := MenuMorph new.
	menu buildTitle: [ :menuTitle | menuTitle
		onlyCloseAndPinable
	].
	menu add: 'apples' selector: #apples.
	menu add: 'oranges' selector: #oranges.
	menu addLine.
	menu add: 'peaches' selector: #peaches.
	menu addLine.
	menu add: 'pears' selector: #pears.
	menu addLine.
	^ menu
	
]

{ #category : #'*GT-SpotterExtensions-Core' }
MenuMorph >> spotterItemsFor: aStep [
	<spotterOrder: 10>
	self flag: #refactoringViolationForSpotterActDefault:.
	^ aStep listProcessor 
		title: 'Items';
		candidatesLimit: Float infinity;
		allCandidates: [ self items ];
		itemName: #contents;
		itemIcon: [ :item | item icon ];
		actLogic: [ :item :step | 
			item hasSubMenu 
				ifTrue: [ step diveIn ] 
				ifFalse: [ item spotterActDefault. step exit ] ];
		filter: GTFilterSubstring 
]
