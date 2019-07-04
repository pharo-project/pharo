"
Description
--------------------

I am a demo showing a demo of tree tables.
"
Class {
	#name : #SpDemoTreeTablePage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Tables'
}

{ #category : #initialization }
SpDemoTreeTablePage class >> pageName [
	^ 'Tree tables'
]

{ #category : #initialization }
SpDemoTreeTablePage >> pageClass [
	^ SpDemoTreeTablePresenter
]
