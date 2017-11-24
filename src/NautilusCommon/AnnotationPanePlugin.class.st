"
An IgorsPlugin is a plugin which displays info about the current selection
"
Class {
	#name : #AnnotationPanePlugin,
	#superclass : #InformationLinePlugin,
	#category : #'NautilusCommon-Plugin'
}

{ #category : #information }
AnnotationPanePlugin class >> description [

	^ 'Display the information of the selected class/method'
]

{ #category : #private }
AnnotationPanePlugin >> buildString [
	| mdl |
	mdl := self model.
	^ String
		streamContents: [ :s | 
			mdl selectedClass
				ifNil: [ '' ]
				ifNotNil: [ :class | 
					mdl selectedMethod
						ifNotNil: [ :method | 
							s
								nextPutAll:
									(AnnotationRequest new
										onCompiledMethod: method;
										annotationRequests: #(#timeStamp #messageCategory #sendersCount #implementorsCount);
										yourself) getAnnotations ] ] ]
]
