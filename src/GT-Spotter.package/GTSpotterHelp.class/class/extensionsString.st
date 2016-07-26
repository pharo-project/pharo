documentation
extensionsString
	| methods title |
	^ String streamContents: [ :s |
		((GTSpotter spotterExtendingMethods groupedBy: #methodClass) 
		associations sorted: [ :a :b | a key name < b key name ]) 
		do: [ :classMethodsAssociation |
			s nextPutAll: classMethodsAssociation key name; cr.
			methods := classMethodsAssociation value.
			methods do: [ :each | 
				title := each ast allChildren 
					detect: [ :node | node isMessage and: [ node selector = #title: ] ]
					ifFound: [ :node | node arguments first value. ]
					ifNone: [ each selector ].
				title isString ifFalse: [ title := each selector ].
				s 
					nextPutAll: '- #'; 
					nextPutAll: title; 
					nextPutAll: ' (in '; 
					nextPutAll: each selector; 
					nextPut: $);
					cr ].
			s cr ] ]