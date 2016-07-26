serialize/materialize
materializeReferencesOf: aPoint with: aDecoder

		aPoint setX: aDecoder nextEncodedReference setY: aDecoder nextEncodedReference.	
