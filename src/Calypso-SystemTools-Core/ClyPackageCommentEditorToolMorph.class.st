"
I am an editor of package comment
 
Internal Representation and Key Implementation Points.

    Instance Variables
	editingPackage:		<RPackage>
"
Class {
	#name : #ClyPackageCommentEditorToolMorph,
	#superclass : #ClyTextEditorToolMorph,
	#instVars : [
		'editingPackage'
	],
	#category : #'Calypso-SystemTools-Core-Editors-Packages'
}

{ #category : #activation }
ClyPackageCommentEditorToolMorph class >> browserTabActivation [
	"This declaration specifies that in any browser when packages are selected, a comment editor will be available in a tab."

	<classAnnotation>
	^ClyTabActivationStrategy for: RPackage asCalypsoItemContext
]

{ #category : #activation }
ClyPackageCommentEditorToolMorph class >> browserTabActivation2 [
	<classAnnotation>
	
	^ClyTabActivationStrategy for: ClyClassGroup asCalypsoItemContext
]

{ #category : #testing }
ClyPackageCommentEditorToolMorph class >> shouldBeActivatedInContext: aBrowserContext [
	^aBrowserContext isPackageSelected and: [aBrowserContext isClassSelected not]
]

{ #category : #activation }
ClyPackageCommentEditorToolMorph class >> tabOrder [
	^ -10
]

{ #category : #operations }
ClyPackageCommentEditorToolMorph >> applyChanges [

	self pendingText asString = self editingText ifFalse: [
		editingPackage packageManifest comment: self pendingText stamp: Author changeStamp].

	^true
]

{ #category : #controlling }
ClyPackageCommentEditorToolMorph >> attachToSystem [

	browser system 
		when: (ClyPackageChange of: editingPackage) send: #triggerUpdate to: self
]

{ #category : #initialization }
ClyPackageCommentEditorToolMorph >> defaultIconName [
	^#commentsIcon
]

{ #category : #initialization }
ClyPackageCommentEditorToolMorph >> defaultTitle [
	^'Comment'
]

{ #category : #controlling }
ClyPackageCommentEditorToolMorph >> detachFromSystem [

	browser system unsubscribe: self
]

{ #category : #accessing }
ClyPackageCommentEditorToolMorph >> editingPackage [
	^ editingPackage
]

{ #category : #accessing }
ClyPackageCommentEditorToolMorph >> editingPackage: anObject [
	editingPackage := anObject
]

{ #category : #accessing }
ClyPackageCommentEditorToolMorph >> editingText [
	^editingPackage packageManifestOrNil 
		ifNil: [ self packageCommentBlank ]
		ifNotNil: [ :manifest | 
			manifest hasComment 
				ifTrue: [ manifest comment ] 
				ifFalse: [ self packageCommentBlank ] ]
]

{ #category : #testing }
ClyPackageCommentEditorToolMorph >> isSimilarTo: anotherBrowserTool [
	(super isSimilarTo: anotherBrowserTool) ifFalse: [ ^false ].
	
	^editingPackage = anotherBrowserTool editingPackage
]

{ #category : #accessing }
ClyPackageCommentEditorToolMorph >> packageCommentBlank [

	^'Please comment package here'
]

{ #category : #printing }
ClyPackageCommentEditorToolMorph >> printContext [
	^editingPackage name
]

{ #category : #initialization }
ClyPackageCommentEditorToolMorph >> setUpModelFromContext [
	super setUpModelFromContext.

	editingPackage := context lastSelectedPackage
]

{ #category : #testing }
ClyPackageCommentEditorToolMorph >> wantsTextStyling [
	^false
]
