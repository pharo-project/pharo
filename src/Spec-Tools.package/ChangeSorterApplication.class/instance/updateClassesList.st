api
updateClassesList
	
	| sel |
	sel := self selectedClass.
	
	self selectedChangeSet
			ifNil: [ classesListModel items: {} ]
			ifNotNil: [:change |	classesListModel items: (change changedClasses sort: [:a :b | a name < b name ])].
				
	classesListModel setSelectedItem: sel