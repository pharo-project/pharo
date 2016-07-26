instance creation
openOn: aClass 
	rootClass := aClass.
	(self dialogWindowIn: World title: 'All Object subclasses') openInWorld
