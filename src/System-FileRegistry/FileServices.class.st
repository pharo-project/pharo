"
Registered file services for specific file types.

To use me you can register a class by adding a method as: 

MyClass class>>fileReaderServicesForFile: fullName suffix: suffix
	<fileService>
	
	^ (FileStream isSourceFileSuffix: suffix)
		ifTrue: [ { self mySimpleServiceEntry1 . self mySimpleServiceEntry2 }]
		ifFalse: [#()]
"
Class {
	#name : #FileServices,
	#superclass : #Object,
	#category : 'System-FileRegistry'
}

{ #category : #accessing }
FileServices class >> itemsForDirectory: aFileDirectory [
	"Answer a list of services appropriate when no file is selected."

	| services |
	services := OrderedCollection new.
	(PragmaCollector filter: [ :pragma | pragma keyword = 'directoryService' ]) reset
		do: [ :each | services addAll: (each methodClass soleInstance perform: each selector with: aFileDirectory) ].
	^ services
]

{ #category : #accessing }
FileServices class >> itemsForFile: fullName [
	"Answer a list of services appropriate for a file of the given full name"

	| services suffix |
	suffix := self suffixOf: fullName.
	services := OrderedCollection new.
	(PragmaCollector filter: [ :pragma | pragma keyword = 'fileService' ])
		reset
		do: [ :each | 
			services
				addAll:
					(each methodClass soleInstance
						perform: each methodSelector
						withArguments:
							{fullName.
							suffix}) ].
	^ services
]

{ #category : #helper }
FileServices class >> suffixOf: aName [
	"Answer the file extension of the given file"
	^ aName
		ifNil: ['']
		ifNotNil: [  aName asFileReference extension asLowercase]
]
