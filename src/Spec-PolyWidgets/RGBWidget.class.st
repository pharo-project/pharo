"
I am a Spec widget  allowing a user to choose a Color by its RGB values and providing a preview.

self example

I am composed of a RGBSliders and an ImagePresenter that display simply a form of the current color.

I provide the following methods
- color a shortcut color to access the current color of the RGBSliders.
- makeNewForm return a fresh copy of the current displayed form.

I provide extentForPreview, a point used to define the size of the preview.
"
Class {
	#name : #RGBWidget,
	#superclass : #ComposablePresenter,
	#instVars : [
		'preview',
		'sliders',
		'extentForPreview'
	],
	#category : #Spec-PolyWidgets-RGBAndShape
}

{ #category : #specs }
RGBWidget class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #preview;
				add: #sliders ];
		yourself
]

{ #category : #example }
RGBWidget class >> example [
	| example |
	example := self new.
	example
		title: 'RGBWidget exampe';
		extent: 300 @ 250;
		openDialogWithSpec.
	^ example
]

{ #category : #api }
RGBWidget >> color [
 ^ sliders color
]

{ #category : #initialization }
RGBWidget >> initializePresenter [
	sliders whenChangedDo: [ preview image: self makeNewForm ].
]

{ #category : #initialization }
RGBWidget >> initializeWidgets [
	preview := self instantiate: ImagePresenter.
	sliders := self instantiate: RGBSliders.
	preview image: (Form extent: 30 @ 30 depth: 32).
	preview image: self makeNewForm.
	self focusOrder add: sliders
]

{ #category : #api }
RGBWidget >> makeNewForm [
	^ (Form extent: 30 @ 30 depth: 32) fillColor: self color
]

{ #category : #accessing }
RGBWidget >> preview [
	^ preview
]

{ #category : #accessing }
RGBWidget >> sliders [
	^ sliders
]

{ #category : #api-events }
RGBWidget >> whenExtentForPreviewChanged: aBlock [
 extentForPreview whenChangedDo: aBlock 
]
