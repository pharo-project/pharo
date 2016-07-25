initialization
initializePresenter
	filterField
		whenTextChanged: [ :text | 
			clearFilterButton enabled: text isNotEmpty.
			self setFilter: text ].
	clearFilterButton action: [ filterField text: '' ]