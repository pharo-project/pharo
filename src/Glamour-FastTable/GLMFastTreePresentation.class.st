"
I'm a fast table presentation who introduces FTTableMorph into Glamour  as a Tree. 

Description
--------------------

I know how to render myself and I store some options for the Tree.  I keep some options specific to the FastTable inside a Dictionary. 

I work with GLMMorphicFastTreeRenderer to render the FastTable. I use TGLMFastTableFunctionsPresentation in order to manage some options.

Public API and Key Messages
--------------------

You can use the public API of my super class. You also use the public API of TGLMFastTableFunctionsPresentation (See his class comment) and you can also use:

- #maxDepth: anInteger 		to define a max depth for the tree.   
		
- #expandedUntil: aDepth 		this is use to expand the tree at the opening until a certain depth.
		 
- #strategy: aSymbole 		this is use to define a Strategy for the search or the filter inside a Tree. I can take in parameter #default, #rootsOnly, #allItems.
                #default is selected by default, it will search/filter the tree for all visible rows.       
               #rootsOnly will search/filter only the first level of the tree
               #allItems will search/filter all the Tree and open the needed items. Do not use this if the tree is not finish.

Example
--------------------

	GLMWrapper new
		show:
				[ :a | 
			a fastTree
				children: [ :x | x subclasses ];
				enableFilterWithAction: [ :dataSource :pattern :item | item browse ];
				strategy: #rootsOnly;
				maxDepth: 3;
				rootsExpanded ];
		openOn: (Object subclasses) asArray
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	parameters:		This is a Dictionary use to store some parameters of the presentation.

"
Class {
	#name : #GLMFastTreePresentation,
	#superclass : #GLMTreePresentation,
	#traits : 'TGLMFastTableFunctionsPresentation',
	#classTraits : 'TGLMFastTableFunctionsPresentation classTrait',
	#instVars : [
		'parameters'
	],
	#category : #'Glamour-FastTable-Presentations'
}

{ #category : #'public api' }
GLMFastTreePresentation >> expandedUntil: anInteger [
	self shouldExpandToLevel: anInteger
]

{ #category : #'public api' }
GLMFastTreePresentation >> hasChildren: aBlock [
	self parameters at: 'hasChildrenBlock' put: aBlock
]

{ #category : #accessing }
GLMFastTreePresentation >> hasChildrenBlock [ 
	^ self parameters at: 'hasChildrenBlock' ifAbsent: [ nil ]
]

{ #category : #initialization }
GLMFastTreePresentation >> initialize [
	super initialize.
	parameters := Dictionary new
]

{ #category : #accessing }
GLMFastTreePresentation >> maxDepth [
	^ self parameters at: 'maxDepth' ifAbsent: [ nil ]
]

{ #category : #'public api' }
GLMFastTreePresentation >> maxDepth: anInteger [
	self parameters at: 'maxDepth' put: anInteger
]

{ #category : #accessing }
GLMFastTreePresentation >> parameters [
	^ parameters
]

{ #category : #rendering }
GLMFastTreePresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderFastTreePresentation: self
]

{ #category : #accessing }
GLMFastTreePresentation >> strategy [
	^ self parameters at: 'strategy' ifAbsent: [ #default ]
]

{ #category : #'public api' }
GLMFastTreePresentation >> strategy: aSymbole [
	self parameters at: 'strategy' put: aSymbole
]
