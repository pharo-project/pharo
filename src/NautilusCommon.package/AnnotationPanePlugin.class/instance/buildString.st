private
buildString
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