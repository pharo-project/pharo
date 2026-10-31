"
I represent a zip file system entry for a directory. Together with ZipFileSystemFile I take care of lazily accessing archive members.
"
Class {
	#name : 'ZipFileSystemDirectory',
	#superclass : 'MemoryFileSystemDirectory',
	#category : 'FileSystem-Zip-Base',
	#package : 'FileSystem-Zip',
	#tag : 'Base'
}

{ #category : 'creation' }
ZipFileSystemDirectory >> ensureCreateFile: aFileName [
	^ self
		fileEntryAt: aFileName
		put: (ZipFileSystemFile named: aFileName)
]

{ #category : '*NewTools-Inspector' }
ZipFileSystemDirectory >> inspectionItems [
	<inspectorPresentationOrder: 1 title: 'Contents'>

	^ SpTablePresenter new
		addColumn: (SpStringTableColumn title: 'Name' evaluated: [ :entry | entry name ]);
		items: self entries values;
		yourself
]