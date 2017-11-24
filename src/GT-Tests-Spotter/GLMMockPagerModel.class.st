Class {
	#name : #GLMMockPagerModel,
	#superclass : #GLMPagerModel,
	#category : #'GT-Tests-Spotter-Scripting-Mocks'
}

{ #category : #panes }
GLMMockPagerModel >> popPane: isSmooth [
	self flag: 'the spotter-model will not pop its step unless the UI allows it by this call - hmmmmm'.
	self notifyPagePoped: nil
]

{ #category : #scrolling }
GLMMockPagerModel >> scrollToPane: pageIndex callback: aBlock [
	self flag: 'this method gets called directly from the gt-spotter model - hmmmmm'.
	aBlock value
]
