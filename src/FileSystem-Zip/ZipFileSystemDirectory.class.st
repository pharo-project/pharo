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

	^ SpTreeTablePresenter new
		roots: self entries values;
		children: [ :aNode | aNode isDirectory ifTrue: [ aNode entries values ] ifFalse: [ #() ] ];
		addColumn: (SpCompositeTableColumn new
			title: 'Name';
			addColumn: (SpImageTableColumn new
				evaluated: [ :aNode | 
					aNode isDirectory 
						ifTrue: [ self iconNamed: #folder ] 
						ifFalse: [ self iconNamed: #file ] ];
				beNotExpandable;
				yourself);
			addColumn: (SpStringTableColumn new evaluated: [ :aNode | aNode basename ]);
			yourself);
		addColumn: (SpStringTableColumn new 
			title: 'Size';
			evaluated: [ :aNode | 
				aNode isDirectory 
					ifTrue: [ '--' ] 
					ifFalse: [ aNode size asString ]];
			yourself);
		yourself
]