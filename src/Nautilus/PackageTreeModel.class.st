"
I'm a tree model for managing groups, packages and tags.
"
Class {
	#name : #PackageTreeModel,
	#superclass : #MorphTreeModel,
	#instVars : [
		'model'
	],
	#category : #'Nautilus-Tree'
}

{ #category : #'drag and drop' }
PackageTreeModel >> acceptDroppingMorph: aMorph event: evt inMorph: aTreeMorph [
	| target items |
	
	target := (aTreeMorph scrollerSubMorphFromPoint: evt position) complexContents item.
	items := aMorph passenger.
	target ifNotNil: [ 
		model 
			dropInAPackage: items 
			into: target ]
]

{ #category : #'keyboard managing' }
PackageTreeModel >> arrowEvent: event key: aChar target: aMorph [
	"Transmit flow to next column"
	^ self model keyPressedOnTree: event target: aMorph.
]

{ #category : #'event handling' }
PackageTreeModel >> doubleClick [
	self selectedNode doubleClick
]

{ #category : #'event handling' }
PackageTreeModel >> doubleClickEvent: anEvent onNodeMorph: aNodeMorph [
	self selectedNode ifNotNil: [ :n | n doubleClickEvent: anEvent onNodeMorph: aNodeMorph ]
]

{ #category : #accessing }
PackageTreeModel >> groupIconFor: aPackage [ 
	^ self model groupIconFor: aPackage.
]

{ #category : #private }
PackageTreeModel >> groupsAsNodes [
	^ (self model groups collect: [ :each | PackageTreePackageGroupNodeModel with: each model: self ])
		sorted: [ :a :b | a name < b name ]
]

{ #category : #'help-text' }
PackageTreeModel >> helpTextFor: aRPackage [
	"We try to get a configuration class with the same name of the package"

	| possibleConfigurationNames |
	aRPackage name ifEmpty: [ ^ nil ].
	possibleConfigurationNames := OrderedCollection new.
	(aRPackage name beginsWith: 'ConfigurationOf')
		ifTrue: [ possibleConfigurationNames addLast: aRPackage name ]
		ifFalse: [ 
			| stream |
			"'ConfigurationOf' size"
			stream := (String new: aRPackage name size + 15) writeStream.
			stream nextPutAll: 'ConfigurationOf'.
			$-
				split: aRPackage name
				do: [ :each | 
					(each = 'Core' or: [ each = 'Tests' ])
						ifFalse: [ 
							stream nextPutAll: each.
							possibleConfigurationNames addLast: stream contents ] ] ].
	"try longeset match first"
	possibleConfigurationNames
		reverseDo: [ :each |
			self class environment at: each asSymbol ifPresent: [ :configuration | 
					"If the configuration has a catalog description we show it, if not we don't"
					(configuration respondsTo: #catalogDescription)
						ifTrue: [ ^ configuration catalogDescription ]
						ifFalse: [ ^ nil ] ] ].
	^ nil.
]

{ #category : #accessing }
PackageTreeModel >> menu: menu shifted: b [
	^ self model menu: menu shifted: b
]

{ #category : #private }
PackageTreeModel >> methodGroupAsNode [
	^ self model model class methodGroupManager groups
		collect: [ :each | PackageTreeMethodGroupNodeModel with: each model: self ]
]

{ #category : #accessing }
PackageTreeModel >> model [
	^ model
]

{ #category : #accessing }
PackageTreeModel >> model: anObject [
	model := anObject
]

{ #category : #updating }
PackageTreeModel >> okToChange [
	^ self model okToChange
]

{ #category : #accessing }
PackageTreeModel >> packageIconFor: aPackage [ 
	^ self model packageIconFor: aPackage.
]

{ #category : #'package filter' }
PackageTreeModel >> packageMatchPattern: aRPackage [ 
	
	^ self model packageMatchPattern: aRPackage
]

{ #category : #private }
PackageTreeModel >> packagesAsNodes [
	^ (self model packages
		select: [ :each | (self packageMatchPattern: each) and: [ ((each name beginsWith: 'ConfigurationOf') or: [ each name beginsWith: 'BaselineOf' ]) not ] ]
		thenCollect: [ :each | each asNautilusNodeWithModel: self ]) sorted: [ :a :b | a name < b name ]
]

{ #category : #accessing }
PackageTreeModel >> rootNodes [
	^ self methodGroupAsNode , self groupsAsNodes , self packagesAsNodes
]

{ #category : #accessing }
PackageTreeModel >> tagIconFor: aTag [ 
	^ nil
]

{ #category : #'drag and drop' }
PackageTreeModel >> wantsDroppedMorph: aMorph event: anEvent inMorph: destMorph [
	"TODO: Filter out to avoid drag and drop of different objet types. Here, we don't know what the target may be so filtering is limited."

	aMorph dragTransferType ifNil: [ ^ false ].
	^ destMorph model == self
]
