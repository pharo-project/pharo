as yet unclassified
openDialogOn: aClass 
	rootClass := aClass.
	^ self openDialogWindowIn: World title: 'All Object subclasses'
