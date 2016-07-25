parsing
parseObject
	| targetClass reference object |
	[
		reference := self newReference.
		targetClass := self parseClass.
		object := targetClass fromSton: self .
		self setReference: reference to: object ]
		on: NotFound 
		do: [ :notFound |
			acceptUnknownClasses 
				ifTrue: [ 
					object := STON mapClass new.
					self storeReference: object.
					self parseMapDo: [ :key :value |
						object at: key put: value ].
					object at: STON classNameKey put: notFound object ]
				ifFalse: [ self error: 'Cannot resolve class named ', notFound object printString ] ].
	^ object