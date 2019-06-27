"
Description
--------------------

I am a demo page to show how to use progress bar.
"
Class {
	#name : #SpProgressBarDemoPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Loading'
}

{ #category : #initialization }
SpProgressBarDemoPage class >> pageName [
	^ 'Progress bar'
]

{ #category : #initialization }
SpProgressBarDemoPage >> pageClass [
	^ SpProgressBarDemo
]
