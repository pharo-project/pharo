add/remove
itemRemoved: aItem

	critics valuesDo: [ :each | each remove: aItem ifAbsent: []].
	toDos valuesDo: [ :each | each remove: aItem ifAbsent: []].
	falsePositives valuesDo: [ :each | each remove: aItem ifAbsent: []].	
	self updateBrowser  