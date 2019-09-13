"
Description
--------------------

This demo show how to use the link presenter
"
Class {
	#name : #SpDemoLinksPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'link1',
		'link2',
		'link3',
		'link4'
	],
	#category : #'Spec2-Examples-Demo-Other'
}

{ #category : #specs }
SpDemoLinksPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #link1;
		add: #link2;
		add: #link3;
		add: #link4;
		yourself
]

{ #category : #'instance creation' }
SpDemoLinksPresenter class >> open [
	<script>
	self new openWithSpec
]

{ #category : #initialization }
SpDemoLinksPresenter >> initializePresenters [
	link1 := self newLink.
	link2 := self newLink.
	link3 := self newLink.
	link4 := self newLink.

	link1 url: 'https://pharo.org'.

	link2
		url: 'https://pharo.org';
		label: 'Pharo website'.

	link3
		url: 'https://pharo.org';
		label: 'Pharo website';
		color: Color purple.

	link4
		action: [ Object browse ];
		label: 'Browse Object'
]
