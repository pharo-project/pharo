"
I'm a fast table presentation who introduces FTTableMorph into Glamour  as a Tree that can have columns. 

Description
--------------------

I know how to render myself and I store some options for the building a tree and for displaying multiple columns .  

I work with GLMMorphicFastTreeWithColumnsRenderer to render the FastTable. I use TGLMScriptingAPITablePresentation in order to adding the API for configuring columns. 

Public API and Key Messages
--------------------

You can use the public API of my super class. You also use the public API of TGLMScriptingAPITablePresentation (See his class comment).

Example
--------------------
	
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastTreeTable 
			children: [ :x | 1 to: x-1 ];
			column: [:x | 'Numbers from 1 to ', x asString] evaluated: #asString;
			column: 'Even' evaluated: [ :each | each even asString ];
			column: 'Odd' evaluated: [ :each | each odd asString ];
			icon: [ :each | 
				each asInteger odd
					ifTrue: [ GLMUIThemeExtraIcons glamorousRedCircle ]
					ifFalse: [ GLMUIThemeExtraIcons glamorousGreenCircle ] ] ].
	browser openOn: (1 to: 10)

Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	columns:		This is an OrderedCollection storing the columns that will be rendered by this presentation.

"
Class {
	#name : #GLMFastTreeTablePresentation,
	#superclass : #GLMFastTreePresentation,
	#traits : 'TGLMScriptingAPITablePresentation',
	#classTraits : 'TGLMScriptingAPITablePresentation classTrait',
	#instVars : [
		'columns'
	],
	#category : #Glamour-FastTable-Presentations
}

{ #category : #accessing }
GLMFastTreeTablePresentation >> columns [
	^ columns ifNil: [ columns := OrderedCollection new ]
]

{ #category : #scripting }
GLMFastTreeTablePresentation >> hideHeader [
	^ self parameters at: 'shouldShowHeader' put: [ false ]
]

{ #category : #scripting }
GLMFastTreeTablePresentation >> intercellSpacing [
	^ self parameters 
		at: 'intercellSpacing' 
		ifAbsent: [ self class defaultIntercellSpacing ]
]

{ #category : #scripting }
GLMFastTreeTablePresentation >> intercellSpacing: aNumber [
	self parameters at: 'intercellSpacing' put: aNumber
]

{ #category : #rendering }
GLMFastTreeTablePresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderFastTreeWithColumnsPresentation: self
]

{ #category : #accessing }
GLMFastTreeTablePresentation >> shouldShowHeader [
	^ self parameters at: 'shouldShowHeader' ifAbsent: [ true ]
]
