"
I am the adapter used to bridge a DialogWindowPresenter and a DialogWindow
"
Class {
	#name : #MorphicDialogWindowAdapter,
	#superclass : #MorphicWindowAdapter,
	#category : 'Spec-MorphicAdapters'
}

{ #category : #factory }
MorphicDialogWindowAdapter >> buildWidget [

	^ SpecDialogWindow new
			specWidget: self contentMorph;
			setToolbarFrom: self toolbarBlock;
			model: self model;
			initialize;
			yourself
]

{ #category : #'spec protocol' }
MorphicDialogWindowAdapter >> cancelAction: aBlock [

	^ self widget
		ifNotNil: [:w | w cancelAction: aBlock ]
]

{ #category : #'widget API' }
MorphicDialogWindowAdapter >> contentMorph [

	^ self model contents
]

{ #category : #'spec protocol' }
MorphicDialogWindowAdapter >> okAction: aBlock [

	^ self widget
		ifNotNil: [:w | w okAction: aBlock ]
]

{ #category : #'spec protocol' }
MorphicDialogWindowAdapter >> okButtonEnabled: aBoolean [

	self widget ifNotNil: [ :w | w toolbar okButton enabled: aBoolean ]
]

{ #category : #protocol }
MorphicDialogWindowAdapter >> open [

	self widget openInWorld
]

{ #category : #'spec protocol' }
MorphicDialogWindowAdapter >> toolbar: aToolbar [ 

	self widgetDo: [ :w | w toolbar: aToolbar ]
]

{ #category : #'widget API' }
MorphicDialogWindowAdapter >> toolbarBlock [

	^ [ self model toolbar ]
]

{ #category : #'spec protocol' }
MorphicDialogWindowAdapter >> triggerCancelAction [

	self widget ifNotNil: [:w | w toolbar triggerCancelAction ]
]

{ #category : #'spec protocol' }
MorphicDialogWindowAdapter >> triggerOkAction [

	self widget ifNotNil: [:w | w toolbar triggerOkAction ]
]
