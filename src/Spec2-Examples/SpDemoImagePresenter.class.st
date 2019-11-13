"
Description
--------------------

This demo show how to use the image presenter
"
Class {
	#name : #SpDemoImagePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'image1',
		'image2'
	],
	#category : #'Spec2-Examples-Demo-Other'
}

{ #category : #specs }
SpDemoImagePresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: (SpBoxLayout newHorizontal
			add: 'Image added through an ImagePresenter:';
			add: #image1;
			yourself)
		height: 50;
		add: (SpBoxLayout newHorizontal
			add: 'Image added through an ImagePresenter:';
			add: #image2;
			yourself);
		add: (SpBoxLayout newHorizontal
			add: 'Image added directly from the layout definition:';
			add: (self iconNamed: #pharoBig);
			yourself);
		yourself
]

{ #category : #'instance creation' }
SpDemoImagePresenter class >> open [
	<script>
	self new openWithSpec
]

{ #category : #accessing }
SpDemoImagePresenter >> image1 [
	^ image1
]

{ #category : #accessing }
SpDemoImagePresenter >> image2 [
	^ image2
]

{ #category : #initialization }
SpDemoImagePresenter >> initializePresenters [
	image1 := self newImage.
	image2 := self newImage.

	image1 image: (self iconNamed: #pharo).
	image2 image: self currentWorld submorphs last form.
]
