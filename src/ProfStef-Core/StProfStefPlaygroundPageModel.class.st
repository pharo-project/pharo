Class {
	#name : 'StProfStefPlaygroundPageModel',
	#superclass : 'StPlaygroundPage',
	#category : 'ProfStef-Core-presenter',
	#package : 'ProfStef-Core',
	#tag : 'presenter'
}

{ #category : 'inspector compatibility' }
StProfStefPlaygroundPageModel >> defaultObjectInspectorClass [
	"Since the playground can be the starting point of an inspection (using <meta+g>), the 
	 playground page (which is the model of a playground) needs to behave as a valid starting point. 
	 This method declares answer the presenter to use in the inspection miller list."

	^ StProfStefPlaygroundPage
]
