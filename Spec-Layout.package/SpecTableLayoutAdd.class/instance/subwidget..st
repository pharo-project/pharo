accessing
subwidget: aSpec
	subwidget := aSpec isSymbol
		ifTrue: [{#model. aSpec}]
		ifFalse: [
			aSpec isCollection
				ifTrue: [
					aSpec isEmpty 
						ifTrue: [ "should raise an error" ]
						ifFalse: [ aSpec first == #model 
										ifTrue: [ aSpec ]
										ifFalse: [ {#model}, aSpec ]]]
				ifFalse: [ 
					"Not a symbol or a collection. We assume it's an object to add directly (like a morph by example)"
					aSpec ]].
		
	self generateArguments