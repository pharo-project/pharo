"
Description
--------------------

This demo show how to use the link presenter
"
Class {
	#name : #SpecDemoLinksPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'link1',
		'link2',
		'link3'
	],
	#category : #'Spec-Examples-Demo'
}

{ #category : #specs }
SpecDemoLinksPresenter class >> defaultSpec [
	^ SpecLayout composed
		newColumn: [ :c | 
			c
				newRow: #link1 height: self labelHeight;
				addSpacer;
				newRow: #link2 height: self labelHeight;
				addSpacer;
				newRow: #link3 height: self labelHeight;
				newRow: [ :r |  ] ];
		yourself
]

{ #category : #'instance creation' }
SpecDemoLinksPresenter class >> open [
	<script>
	self new openWithSpec
]

{ #category : #initialization }
SpecDemoLinksPresenter >> initializeWidgets [
	link1 := self newLink.
	link2 := self newLink.
	link3 := self newLink.

	link1 url: 'https://pharo.org'.

	link2
		url: 'https://pharo.org';
		label: 'Pharo website'.

	link3
		url: 'https://pharo.org';
		label: 'Pharo website';
		color: Color purple
]

{ #category : #accessing }
SpecDemoLinksPresenter >> link1 [
	^ link1
]

{ #category : #accessing }
SpecDemoLinksPresenter >> link2 [
	^ link2
]

{ #category : #accessing }
SpecDemoLinksPresenter >> link3 [
	^ link3
]
