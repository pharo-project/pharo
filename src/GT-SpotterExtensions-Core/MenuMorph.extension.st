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
