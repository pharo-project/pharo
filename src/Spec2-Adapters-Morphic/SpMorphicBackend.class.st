Class {
	#name : #SpMorphicBackend,
	#superclass : #SpApplicationBackend,
	#category : #'Spec2-Adapters-Morphic-Backend'
}

{ #category : #accessing }
SpMorphicBackend class >> backendName [

	^ #Morphic
]

{ #category : #private }
SpMorphicBackend >> adapterBindingsClass [

	^ SpMorphicAdapterBindings
]

{ #category : #accessing }
SpMorphicBackend >> defer: aBlock [

	UIManager default defer: aBlock
]

{ #category : #'private notifying' }
SpMorphicBackend >> notifyError: aSpecNotification [
	GrowlMorph
		openWithLabel: 'Error'
		contents: aSpecNotification message
		backgroundColor: GrowlMorph theme dangerBackgroundColor
		labelColor: GrowlMorph theme textColor
]

{ #category : #'private notifying' }
SpMorphicBackend >> notifyInfo: aSpecNotification [

	GrowlMorph 
		openWithLabel: 'Information' translated 
		contents: aSpecNotification message
]

{ #category : #'ui dialogs' }
SpMorphicBackend >> selectDirectoryTitle: aString [

	^ UIManager default 
		chooseDirectory: aString
		path: ''
]

{ #category : #'ui dialogs' }
SpMorphicBackend >> selectFileTitle: aString [

	^ UIManager default 
		chooseExistingFileReference: aString
		extensions: nil 
		path: ''
]
