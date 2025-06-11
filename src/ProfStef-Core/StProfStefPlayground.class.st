Class {
	#name : 'StProfStefPlayground',
	#superclass : 'StPlaygroundPresenter',
	#category : 'ProfStef-Core-presenter',
	#package : 'ProfStef-Core',
	#tag : 'presenter'
}

{ #category : 'private - factory' }
StProfStefPlayground >> newDefaultPlaygroundPage [

	^ StProfStefPlaygroundPageModel new
]

{ #category : 'initialization' }
StProfStefPlayground >> windowTitle [
	^ 'Prof Stef'
]
