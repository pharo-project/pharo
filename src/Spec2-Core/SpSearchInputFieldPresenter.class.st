"
I am a specific text field used to search.
I display an additional ""clear"" icon.
"
Class {
	#name : #SpSearchInputFieldPresenter,
	#superclass : #SpTextInputFieldPresenter,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpSearchInputFieldPresenter class >> adapterName [

	^ #SearchInputFieldAdapter
]

{ #category : #example }
SpSearchInputFieldPresenter class >> example [
	<sampleInstance>
	
	^ self new
		extent: 300 @ 25;
		openWithSpec;
		placeholder: 'Search...'
]
