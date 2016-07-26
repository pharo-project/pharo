updating
updateableBrowser
	<glmBrowser: 'Updateable browser' input: '|collection|
	collection := GLMAnnouncingCollection new.
	collection add: 1.
	collection'>
	"
	|collection|
	collection := GLMAnnouncingCollection new.
	collection add: 1.
	self new updateableBrowser openOn: collection."
	
	| browser |
	browser := GLMTabulator new.
	browser column: #preview.
	browser title: 'Updatable browser'.
	browser 
		act: [:b | b entity add: (b entity size + 1). b update; updateToolbar. ] 	
		icon: GLMUIThemeExtraIcons glamorousAdd
		entitled: 'Add an item in the collection'.
	browser 
		act: [:b | b entity removeLast. b update; updateToolbar. ] 
		icon: GLMUIThemeExtraIcons glamorousRemove
		entitled: 'Remove last item from the collection'.
	browser 
		updateOn: GLMItemAdded from: #yourself;
		updateOn: GLMItemRemoved from: #yourself.
	browser transmit to: #preview; andShow: [ :a |
		a list 
			title: 'List';
			when: [:collection | collection size > 1 ].
		a text 
			title: 'Text';
			format: [:collection | 'Current collection: ', collection printString, '
Add more items to show the list'] ].
	^ browser