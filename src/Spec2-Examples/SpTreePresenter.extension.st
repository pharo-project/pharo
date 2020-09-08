Extension { #name : #SpTreePresenter }

{ #category : #'*Spec2-Examples' }
SpTreePresenter class >> example [
	<sampleInstance>
	
	^ self new
		roots: { Object };
		children: [ :aClass | aClass subclasses ];
		icon: [ :aClass | aClass systemIcon ];
		display: [ :aClass | aClass name ];
		openWithSpec
]

{ #category : #'*Spec2-Examples' }
SpTreePresenter class >> exampleWithContextMenu [
	<sampleInstance>
	| tree |
	
	^ (tree := self new)
		roots: { Object };
		children: [ :aClass | aClass subclasses ];
		icon: [ :aClass | aClass systemIcon ];
		display: [ :aClass | aClass name ];
		contextMenu: [
			SpMenuPresenter new 
				addGroup: [ :group | group 
					addItem: [ :item | item name: tree selectedItem asString ] ] ];
		openWithSpec
]
