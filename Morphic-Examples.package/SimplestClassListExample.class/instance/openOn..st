instance creation
openOn: aClass 
	rootClass := aClass.
	(self defaultTreeMorph buildContents embeddedInMorphicWindowLabeled: 'test') openInWorld
	"(self dialogWindowIn: World title: 'All Object subclasses') openInWorld"
