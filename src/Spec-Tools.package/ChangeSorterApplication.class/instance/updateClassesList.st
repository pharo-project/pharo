api
updateClassesList
	
	| sel |
	sel := self selectedClass.
	
	self selectedChangeSet
			ifNil: [ classesListPresenter items: {} ]
			ifNotNil: [:change |	classesListPresenter items: (change changedClasses sort: [:a :b | a name < b name ])].
				
	classesListPresenter setSelectedItem: sel