"
I'm a filter function to be used by subclassing and implementing #matches: method.
I filter tree leafs, keeping the parent branches alive to show where selected elements come from.

To work, the elements of the tree I use need to be instances of the IceTipTreeNode hierarchy.
"
Class {
	#name : #IceTipTreeFilter,
	#superclass : #IceTipFilter,
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #execution }
IceTipTreeFilter >> execute [
	| root |
	
	root := IceTipTreeGroup new.
	self dataSource rootItem children do: [ :eachRoot |
		eachRoot data traversingDo: [ :each | 
			each isLeaf ifTrue: [ 
				(self matches: each) ifTrue: [ 
					(root ensurePath: each path allButLast) addChild: each ] ] ] ].
	"clean parent because they are roots (otherwise it will broke a 2nd filter tour)"
	root children do: [ :each | each parent: nil ].
	^ self dataSource copy
		rootForItems: root children;
		yourself
]

{ #category : #private }
IceTipTreeFilter >> findOrCreatePath: parents on: root [
	| path next  |
	
	path := parents first.
	next := root children
		detect: [ :each | each data = path data ]
		ifNone: [ root addChild: path toStaticItem ].
	^ parents allButFirst 
		ifNotEmpty: [ :remaining | self findOrCreatePath: remaining on: next ]		
		ifEmpty: [ next ]
	
]
