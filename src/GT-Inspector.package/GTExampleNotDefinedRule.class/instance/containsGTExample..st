running
containsGTExample: aClass

	^ aClass class methods anySatisfy: [:aMethod | 
			aMethod pragmas 
				anySatisfy: [ :pragma | pragma isGTExamplePragma ]]