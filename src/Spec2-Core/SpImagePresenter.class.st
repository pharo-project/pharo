"
An ImagePresenter is a spec presenter for images.
See AbstractWidgetPresenter

I provide the following variables and their accessors
- action is the action performed when I am displayed.
- image is the form i display.
"
Class {
	#name : #SpImagePresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#image => SpObservableSlot',
		'#action => SpObservableSlot',
		'#autoScale => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpImagePresenter class >> adapterName [

	^ #ImageAdapter
]

{ #category : #api }
SpImagePresenter >> action [

	^ action
]

{ #category : #api }
SpImagePresenter >> action: aBlock [
	"Set the action of the image"
	action := aBlock
]

{ #category : #api }
SpImagePresenter >> autoScale [
	^ autoScale
]

{ #category : #api }
SpImagePresenter >> autoScale: aBoolean [
	autoScale := aBoolean
]

{ #category : #api }
SpImagePresenter >> image [

	^ image
]

{ #category : #api }
SpImagePresenter >> image: aForm [
	
	image := aForm
]

{ #category : #initialization }
SpImagePresenter >> initialize [
	super initialize.
	action := [  ].
	autoScale := false
]

{ #category : #api }
SpImagePresenter >> switchAutoscale [
	autoScale := autoScale not.

]

{ #category : #events }
SpImagePresenter >> whenAutoScaleChangeDo: aBlockClosure [
	self property: #autoScale whenChangedDo: aBlockClosure
]

{ #category : #events }
SpImagePresenter >> whenImageChangeDo: aBlockClosure [ 
	
	self property: #image whenChangedDo: aBlockClosure 
]
