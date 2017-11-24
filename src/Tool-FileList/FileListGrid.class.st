"
A grid for a list of files
"
Class {
	#name : #FileListGrid,
	#superclass : #MorphTreeModel,
	#instVars : [
		'parent',
		'treeMorph'
	],
	#category : #'Tool-FileList'
}

{ #category : #menus }
FileListGrid >> fileListMenu: aMenuMorph [ 
	^ parent fileListMenu: aMenuMorph
]

{ #category : #accessing }
FileListGrid >> parent [
	^ parent
]

{ #category : #accessing }
FileListGrid >> parent: aFileList [
	parent := aFileList
]

{ #category : #accessing }
FileListGrid >> rootItems [
	^ parent fileList
]

{ #category : #accessing }
FileListGrid >> rootNodeClassFromItem: anItem [
	^ FileListGridNode
]

{ #category : #'user interface' }
FileListGrid >> treeMorph [
	treeMorph ifNil: [ 
		treeMorph := (self treeMorphClass on: self)
			columns: {
				MorphTreeColumn new
					rowMorphGetSelector: #fileName;
					startWidth: 300;
					headerButtonLabel: 'File name' translated 
						font: nil 
						target: self parent 
						actionSelector: #setSortSelector: arguments: { #sortBlockBasename }.
				MorphTreeColumn new
					rowMorphGetSelector: #fileSize;
					headerButtonLabel: 'Size' translated font: nil 
						target: self parent 
						actionSelector: #setSortSelector: arguments: { #sortBlockFilesize }.
				MorphTreeColumn new
					rowMorphGetSelector: #modificationDate;
					headerButtonLabel: 'Last mod.' translated font: nil  
						target: self parent 
						actionSelector: #setSortSelector: arguments: { #sortBlockTimestamp }.
				MorphTreeColumn new
					rowMorphGetSelector: #filePermissions;
					headerButtonLabel: 'Permissions' translated font: nil  
						target: self parent 
						actionSelector: #setSortSelector: arguments: { #sortBlockPermissions }}.
		treeMorph 
			hResizing: #spaceFill; 
			vResizing: #spaceFill; 
			columnInset: 3; 
			getMenuSelector: #fileListMenu:;
			makeLastColumnUnbounded.
		].

	^ treeMorph buildContents 
]
