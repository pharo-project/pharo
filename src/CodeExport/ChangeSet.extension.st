Extension { #name : 'ChangeSet' }

{ #category : '*CodeExport' }
ChangeSet >> fileOut [
	"File out the receiver, to a file whose name is a function of the
	change-set name and a unique numeric tag."

	| fileReference dir |
	dir := self class promptForDefaultChangeSetDirectoryIfNecessary.
	fileReference := ( dir / self name , 'cs') nextVersion.
	self fileOutBody: fileReference
]

{ #category : '*CodeExport' }
ChangeSet >> fileOutBody: fileReference [

			| internalStream |
			internalStream := (String new: 10000) writeStream.
			internalStream
				header;
				timeStamp.
			self fileOutPreambleOn: internalStream.
			self fileOutOn: internalStream.
			self fileOutPostscriptOn: internalStream.

			CodeExporter
				writeSourceCodeFrom: internalStream
				toFileReference: fileReference
]

{ #category : '*CodeExport' }
ChangeSet >> fileOutChangesFor: class on: stream [
	"Write out all the method changes for this class."

	| changes |
	changes := Set new.
	(self methodChangesAtClass: class name)
		associationsDo: [ :mAssoc |
			(mAssoc value = #remove or: [ mAssoc value = #addedThenRemoved ])
				ifFalse: [ changes add: mAssoc key ] ].
	changes isEmpty
		ifTrue: [ ^ self ].
	class fileOutChangedMessages: changes on: stream.
	stream cr
]

{ #category : '*CodeExport' }
ChangeSet >> fileOutClassDefinition: class on: stream [
	"Write out class definition for the given class on the given stream, if the class definition was added or changed."

	(self atClass: class includes: #rename)
		ifTrue: [
			stream
				nextChunkPut: 'Smalltalk renameClassNamed: #' , (self oldNameFor: class) , ' as: #' , class name;
				cr ].
	(self atClass: class includes: #change)
		ifTrue: [
			"fat definition only needed for changes"
			stream
				nextChunkPut: class definitionString;
				cr.
			DeepCopier new checkClass: class	"If veryDeepCopy weakly copies some inst
			vars in this class, warn author when new ones are added." ]
		ifFalse: [
			(self atClass: class includes: #add)
				ifTrue: [
					"use current definition for add"
					stream
						nextChunkPut: class definition;
						cr.
					DeepCopier new checkClass: class	"If veryDeepCopy weakly copies some inst
				vars in this class, warn author when new ones are added." ] ].
	(self atClass: class includes: #comment)
		ifFalse: [ ^ self ].
	class instanceSide putCommentOnFile: stream.
	stream cr
]

{ #category : '*CodeExport' }
ChangeSet >> fileOutOn: stream [
	"Write out all the changes the receiver knows about"

	| classList traits classes traitList list |
	self isEmpty ifTrue: [self inform: 'Warning: no changes to file out'].

	traits := self changedClasses select: [:each | each isTrait].
	classes := self changedClasses select: [:each | each isBehavior & each isTrait not].
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
		[:aClassName | stream nextChunkPut: 'Smalltalk removeClassNamed: #', aClassName; cr]
]

{ #category : '*CodeExport' }
ChangeSet >> fileOutPSFor: class on: stream [
	"Write out removals and initialization for this class."

	| dict classRecord currentDef |
	classRecord := changeRecords at: class name ifAbsent: [ ^ self ].
	dict := classRecord methodChangeTypes.
	dict keysSortedSafely
		do: [ :key |
			| changeType |
			changeType := dict at: key.
			(#(#remove #addedThenRemoved) includes: changeType)
				ifTrue: [
					stream
						nextChunkPut: class name , ' removeSelector: ' , key storeString;
						cr ]
				ifFalse: [
					(key = #initialize and: [ class isMeta ])
						ifTrue: [
							stream
								nextChunkPut: class soleInstance name , ' initialize';
								cr ] ] ].
	((classRecord includesChangeType: #change) and: [ (currentDef := class definitionString) ~= class definitionString ])
		ifTrue: [
			stream
				nextChunkPut: currentDef;
				cr ].
	(classRecord includesChangeType: #reorganize)
		ifFalse: [ ^ self ].
	class fileOutProtocolsOn: stream.
	stream cr
]

{ #category : '*CodeExport' }
ChangeSet >> fileOutPostscriptOn: stream [
	"If the receiver has a postscript, put it out onto the stream.  "

	| aString |
	aString := self postscriptString.
	(aString isNotNil and: [ aString notEmpty ])
		ifFalse: [ ^ self ].
	stream nextChunkPut: aString.	"surroundedBySingleQuotes"
	stream
		cr;
		cr
]

{ #category : '*CodeExport' }
ChangeSet >> fileOutPreambleOn: stream [
	"If the receiver has a preamble, put it out onto the stream.  "

	| aString |
	aString := self preambleString.
	(aString isNotNil and: [ aString notEmpty ])
		ifFalse: [ ^ self ].
	stream nextChunkPut: aString.	"surroundedBySingleQuotes"
	stream
		cr;
		cr
]

{ #category : '*CodeExport' }
ChangeSet class >> promptForDefaultChangeSetDirectoryIfNecessary [
	<script>

	| path |
	path := UIManager default
		chooseDirectory: 'Please select where you want to save your cs'
		from: FileSystem workingDirectory.
	self defaultChangeSetDirectoryName: path asFileReference.
	^ path asFileReference
]
