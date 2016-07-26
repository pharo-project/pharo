filein/out
fileOutOn: stream 
	"Write out all the changes the receiver knows about"

	| classList traits classes traitList list |
	(self isEmpty and: [stream isKindOf: FileStream])
		ifTrue: [self inform: 'Warning: no changes to file out'].
		
	traits := self changedClasses reject: [:each | each isBehavior].
	classes := self changedClasses select: [:each | each isBehavior].
	traitList := self class traitsOrder: traits asOrderedCollection.
	classList := self class classesOrder: classes asOrderedCollection.
	list := OrderedCollection new
		addAll: traitList;
		addAll: classList;
		yourself.
	
	"First put out rename, max classDef and comment changes."
	list do: [:aClass | self fileOutClassDefinition: aClass on: stream].

	"Then put out all the method changes"
	list do: [:aClass | self fileOutChangesFor: aClass on: stream].

	"Finally put out removals, final class defs and reorganization if any"
	list reverseDo: [:aClass | self fileOutPSFor: aClass on: stream].

	self classRemoves asSortedCollection do:
		[:aClassName | stream nextChunkPut: 'Smalltalk removeClassNamed: #', aClassName; cr].