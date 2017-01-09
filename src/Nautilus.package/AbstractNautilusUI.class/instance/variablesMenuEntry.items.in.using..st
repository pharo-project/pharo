button-variables
variablesMenuEntry: aClass items: aCollection in: aMenu using: aBlock
	| menu |
	aCollection ifEmpty: [ ^ self ].
	menu := aMenu.
	aCollection size < 7
		ifTrue: [ menu
				addLine;
				addMorphBack: (aClass name asStringMorph color: Color gray).
			aCollection sorted
				do: [ :variable | aBlock value: variable value: aClass value: menu ] ]
		ifFalse: [ | groupsSize groups sorted leftover |
			groupsSize := 20.
			leftover := aCollection size \\ groupsSize.
			sorted := aCollection sorted.
			menu addLine.
			groups := (sorted
				groupsOf: groupsSize
				atATimeCollect: [ :each | each ]) asOrderedCollection.
			leftover > 0
				ifTrue: [ groups add: (sorted last: leftover) ].
			groups
				withIndexDo: [ :group :groupIndex | 
					| itemName firstLetter lastLetter |
					firstLetter := (group first, ' ') first asString.
					lastLetter := (group last, ' ') first asString.
					itemName :=  groups size > 1 
						ifTrue: [ aClass name, ' (', firstLetter, '-', lastLetter, ')' ]
						ifFalse: [ aClass name ].					
					menu := MenuMorph new
						defaultTarget: aMenu defaultTarget;
						color: aMenu color;
						yourself.
					aMenu
						add: itemName subMenu: menu.
					aMenu lastItem emphasis: 1.
					group
						do: [ :variable | aBlock value: variable value: aClass value: menu ] ] ]