"
I am a layout used for AbstractWidgetPresenters. I will manage all presenter that need to be adapted via a spec adapter for a specific binding.

Examples
--------------------

	SpecAbstractWidgetLayout for: #ListAdapter
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	adapterName:		<aSymbol>		Key to use in order to find the real adapter of a presenter for a specific binding.

"
Class {
	#name : #SpecAbstractWidgetLayout,
	#superclass : #Object,
	#instVars : [
		'adapterName'
	],
	#category : #'Spec-Layout-Base'
}

{ #category : #'instance creation' }
SpecAbstractWidgetLayout class >> for: aSymbol [
	^ self new
		adapterName: aSymbol;
		yourself
]

{ #category : #building }
SpecAbstractWidgetLayout >> adapt: aPresenter bindings: adapterBindings [
	| adapter |
	adapter := self adapterFor: aPresenter bindings: adapterBindings.
	adapter adapt: aPresenter.
	^ adapter
]

{ #category : #building }
SpecAbstractWidgetLayout >> adapterFor: aPresenter bindings: adapterBindings [
	^ self class environment
		at: (adapterBindings translateSymbol: self adapterName)
		ifPresent: [ :class | class owner: aPresenter ]
		ifAbsent: [ self error: 'No binding for ' , self adapterName , ' found.' ]
]

{ #category : #accessing }
SpecAbstractWidgetLayout >> adapterName [
	^ adapterName
]

{ #category : #accessing }
SpecAbstractWidgetLayout >> adapterName: anObject [
	adapterName := anObject
]

{ #category : #accessing }
SpecAbstractWidgetLayout >> asSpecLayout [
	^ self
]

{ #category : #building }
SpecAbstractWidgetLayout >> buildAdapterFor: aPresenter bindings: adapterBindings [
	"In case we are building an adapter, we need to get the actual adapter class for a specific binding and make it adapt the presenter."

	^ aPresenter needRebuild
		ifTrue: [ self adapt: aPresenter bindings: adapterBindings ]
		ifFalse: [ aPresenter needRebuild: true.
			aPresenter adapter
				ifNil: [ self adapt: aPresenter bindings: adapterBindings ]
				ifNotNil: [ :adapter | 
					adapter isRedrawable ifTrue: [ adapter removeSubWidgets ].
					adapter ] ]
]
