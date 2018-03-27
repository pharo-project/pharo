"
An ImagePresenter is a spec presenter for images.
See AbstractWidgetPresenter

I provide the following variables and their accessors
- action is the action performed when I am displayed.
- image is the form i display.
"
Class {
	#name : #ImagePresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'imageHolder',
		'actionHolder',
		'autoScaleHolder'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
ImagePresenter class >> adapterName [

	^ #ImageAdapter
]

{ #category : #specs }
ImagePresenter class >> defaultSpec [
	<spec>
	
	^ #(ImageAdapter
		adapt: #(model))
]

{ #category : #api }
ImagePresenter >> action [
	"<api: #inspect>"

	^ actionHolder value
]

{ #category : #api }
ImagePresenter >> action: aBlock [
	"Set the action of the image"
	"<api: #block getter: #action>"
	actionHolder value: aBlock
]

{ #category : #'api-valueHolder' }
ImagePresenter >> actionHolder [
	"To be able to register action when the action is changed."
	^ actionHolder
]

{ #category : #api }
ImagePresenter >> autoScale [
	"<api: #inspect>"

	^ autoScaleHolder value
]

{ #category : #'api-valueHolder' }
ImagePresenter >> autoScaleHolder [
	"To be able to register action when the autoscale is changed."

	^ autoScaleHolder
]

{ #category : #api }
ImagePresenter >> image [
	"<api: #inspect>"

	^ imageHolder value
]

{ #category : #api }
ImagePresenter >> image: aForm [
	"<api: #image getter: #image>"
	"Set the form of the imagemorph"
	
	imageHolder value: aForm
]

{ #category : #'api-valueHolder' }
ImagePresenter >> imageHolder [
	"To be able to register action when the image is changed."
	^ imageHolder
]

{ #category : #initialization }
ImagePresenter >> initialize [
	super initialize.
	imageHolder := nil asValueHolder.
	actionHolder := [] asValueHolder. 
	autoScaleHolder := false asValueHolder.
	imageHolder whenChangedDo: [ self changed: #getImage ].
]

{ #category : #api }
ImagePresenter >> switchAutoscale [
	autoScaleHolder value: autoScaleHolder value not.
	self changed: #switchAutoscale with: {}
]
